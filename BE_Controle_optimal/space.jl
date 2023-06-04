module Space

    using Plots
    using Plots.PlotMeasures
    using SparseArrays
    using Printf
    using DifferentialEquations
    using LinearAlgebra # norm
    
    export animation

    # --------------------------------------------------------------------------------------------
    x0     = [-42272.67, 0, 0, -5796.72] # état initial
    μ      = 5.1658620912*1e12
    rf     = 42165.0
    t0     = 0
    global γ_max  = nothing
    global F_max  = nothing

    # Maximizing control
    function control(p)
        u    = zeros(eltype(p),2)
        u[1] = p[3]*γ_max/norm(p[3:4])
        u[2] = p[4]*γ_max/norm(p[3:4])
        return u
    end

    # --------------------------------------------------------------------------------------------
    # Données pour la trajectoire durant le transfert
    mutable struct Transfert
        initial_adjoint
        duration
        flow
    end

    # données pour la trajectoire pré-transfert
    mutable struct PreTransfert
        initial_point
        duration
    end

    # données pour la trajectoire post-transfert
    mutable struct PostTransfert
        duration
    end
    
    # --------------------------------------------------------------------------------------------
    # Default options for flows
    # --------------------------------------------------------------------------------------------
    function __abstol()
        return 1e-10
    end

    function __reltol()
        return 1e-10
    end

    function __saveat()
        return []
    end

    # --------------------------------------------------------------------------------------------
    #
    # Vector Field
    #
    # --------------------------------------------------------------------------------------------
    struct VectorField f::Function end

    function (vf::VectorField)(x) # https://docs.julialang.org/en/v1/manual/methods/#Function-like-objects
       return vf.f(x)
    end
    
    # Flow of a vector field
    function Flow(vf::VectorField)
        
        function rhs!(dx, x, dummy, t)
            dx[:] = vf(x)
        end
        
        function f(tspan, x0; abstol=__abstol(), reltol=__reltol(), saveat=__saveat())
            ode = ODEProblem(rhs!, x0, tspan)
            sol = solve(ode, Tsit5(), abstol=abstol, reltol=reltol, saveat=saveat)
            return sol
        end
        
        function f(t0, x0, t; abstol=__abstol(), reltol=__reltol(), saveat=__saveat())
            sol = f((t0, t), x0; abstol=abstol, reltol=reltol, saveat=saveat)
            n = size(x0, 1)
            return sol[1:n, end]
        end
        
        return f
    
    end

    # Kepler's law
    function kepler(x)
        n     = size(x, 1)
        dx    = zeros(eltype(x), n)
        x1    = x[1]
        x2    = x[2]
        x3    = x[3]
        x4    = x[4]
        dsat  = norm(x[1:2]); r3 = dsat^3;
        dx[1] =  x3
        dx[2] =  x4
        dx[3] = -μ*x1/r3
        dx[4] = -μ*x2/r3
        return dx
    end

    KeplerFlow = Flow(VectorField(kepler));
    
    function animation(p0, tf, f, γ_max; fps=10, nFrame=100)
        #
        t_pre_transfert = 2*5.302395297743802
        x_pre_transfert = x0
        pre_transfert_data = PreTransfert(x_pre_transfert, t_pre_transfert)
        #
        transfert_data = Transfert(p0, tf, f)
        #
        t_post_transfert = 20
        post_transfert_data = PostTransfert(t_post_transfert)
        #
        __animation(pre_transfert_data, transfert_data, post_transfert_data, KeplerFlow, γ_max, fps=fps, nFrame=nFrame)
    end

    function __animation(pre_transfert_data, transfert_data, post_transfert_data, f0, _γ_max; fps=10, nFrame=200)
    
        #
        global F_max = _γ_max*2000*10^3/3600^2
        global γ_max = _γ_max

        # pré-transfert
        t_pre_transfert = pre_transfert_data.duration
        x_pre_transfert = pre_transfert_data.initial_point
        
        # transfert
        p0_transfert = transfert_data.initial_adjoint
        tf_transfert = transfert_data.duration
        f            = transfert_data.flow
        
        # post-trasfert
        t_post_transfert = post_transfert_data.duration
        
        # On calcule les orbites initiale et finale
        r0        = norm(x0[1:2])
        v0        = norm(x0[3:4])
        a         = 1.0/(2.0/r0-v0*v0/μ)
        t1        = r0*v0*v0/μ - 1.0;
        t2        = (x0[1:2]'*x0[3:4])/sqrt(a*μ);
        e_ellipse = norm([t1 t2])
        p_orb     = a*(1-e_ellipse^2);
        n_theta   = 151
        Theta     = range(0.0, stop=2*pi, length=n_theta)
        X1_orb_init = zeros(n_theta)
        X2_orb_init = zeros(n_theta)
        X1_orb_arr  = zeros(n_theta)
        X2_orb_arr  = zeros(n_theta)
    
        for  i in 1:n_theta
            theta = Theta[i]
            r_orb = p_orb/(1+e_ellipse*cos(theta));
            # Orbite initiale
            X1_orb_init[i] = r_orb*cos(theta);
            X2_orb_init[i] = r_orb*sin(theta);
            # Orbite finale
            X1_orb_arr[i] = rf*cos(theta) ;
            X2_orb_arr[i] = rf*sin(theta);
        end
        
        # Centre de la fenêtre
        cx = 40000
        cy = -7000
    
        # Taille de la fenêtre
        w = 1600
        h = 900
        ρ = h/w
        
        # Limites de la fenêtre
        ee = 0.5
        xmin = minimum([X1_orb_init; X1_orb_arr]); xmin = xmin - ee * abs(xmin);
        xmax = maximum([X1_orb_init; X1_orb_arr]); xmax = xmax + ee * abs(xmax);
        ymin = minimum([X2_orb_init; X2_orb_arr]); ymin = ymin - ee * abs(ymin);
        ymax = maximum([X2_orb_init; X2_orb_arr]); ymax = ymax + ee * abs(ymax);
        
        Δy = ymax-ymin
        Δx = Δy/ρ
        Δn = Δx - (xmax-xmin)
        xmin = xmin - Δn/2
        xmax = xmax + Δn/2
        
        # Trajectoire pré-transfert
        traj_pre_transfert  = f0((0.0, t_pre_transfert), x_pre_transfert)
        times_pre_transfert  = traj_pre_transfert.t
        n_pre_transfert  = size(times_pre_transfert, 1)
        x1_pre_transfert = [traj_pre_transfert[1, j] for j in 1:n_pre_transfert ]
        x2_pre_transfert = [traj_pre_transfert[2, j] for j in 1:n_pre_transfert ]
        v1_pre_transfert = [traj_pre_transfert[3, j] for j in 1:n_pre_transfert ]
        v2_pre_transfert = [traj_pre_transfert[4, j] for j in 1:n_pre_transfert ]  
        
        # Trajectoire pendant le transfert
        traj_transfert  = f((t0, tf_transfert), x0, p0_transfert)
        times_transfert  = traj_transfert.t
        n_transfert  = size(times_transfert, 1)
        x1_transfert = [traj_transfert[1, j] for j in 1:n_transfert ]
        x2_transfert = [traj_transfert[2, j] for j in 1:n_transfert ]
        v1_transfert = [traj_transfert[3, j] for j in 1:n_transfert ]
        v2_transfert = [traj_transfert[4, j] for j in 1:n_transfert ]
        u_transfert  = zeros(2, length(times_transfert))
        for j in 1:n_transfert
            u_transfert[:,j] = control(traj_transfert[5:8, j])
        end
        u_transfert = u_transfert./γ_max  # ||u|| ≤ 1
    
        # post-transfert
        x_post_transfert = traj_transfert[:,end]
    
        # Trajectoire post-transfert
        traj_post_transfert  = f0((0.0, t_post_transfert), x_post_transfert)
        times_post_transfert  = traj_post_transfert.t
        n_post_transfert  = size(times_post_transfert, 1)
        x1_post_transfert = [traj_post_transfert[1, j] for j in 1:n_post_transfert ]
        x2_post_transfert = [traj_post_transfert[2, j] for j in 1:n_post_transfert ]
        v1_post_transfert = [traj_post_transfert[3, j] for j in 1:n_post_transfert ]
        v2_post_transfert = [traj_post_transfert[4, j] for j in 1:n_post_transfert ] 
    
        # Angles de rotation du satellite pendant le pré-transfert
        # Et poussée normalisée entre 0 et 1
        θ0 = atan(u_transfert[2, 1], u_transfert[1, 1])
        θ_pre_transfert = range(π/2, mod(θ0, 2*π), length=n_pre_transfert)
        F_pre_transfert = zeros(1, n_pre_transfert)
    
        # Angles de rotation du satellite pendant le transfert
        θ_transfert = atan.(u_transfert[2, :], u_transfert[1, :])
        F_transfert = zeros(1, n_transfert)
        for j in 1:n_transfert
            F_transfert[j] = norm(u_transfert[:,j])
        end
    
        # Angles de rotation du satellite pendant le post-transfert
        θ1 = atan(u_transfert[2, end], u_transfert[1, end])
        θ2 = atan(-x2_post_transfert[end], -x1_post_transfert[end])
        θ_post_transfert = range(mod(θ1, 2*π), mod(θ2, 2*π), length=n_post_transfert)
        F_post_transfert = zeros(1, n_post_transfert)
    
        # Etoiles
        Δx = xmax-xmin
        Δy = ymax-ymin 
        ρ  = Δy/Δx
        S = stars(ρ)
    
        # nombre total d'images
        nFrame = min(nFrame, n_pre_transfert+n_transfert+n_post_transfert);
        
        # Pour l'affichage de la trajectoire globale
        times = [times_pre_transfert[1:end-1];
            times_pre_transfert[end].+times_transfert[1:end-1];
            times_pre_transfert[end].+times_transfert[end].+times_post_transfert[1:end]]
        x1 = [x1_pre_transfert[1:end-1]; x1_transfert[1:end-1]; x1_post_transfert[:]]
        x2 = [x2_pre_transfert[1:end-1]; x2_transfert[1:end-1]; x2_post_transfert[:]]
        v1 = [v1_pre_transfert[1:end-1]; v1_transfert[1:end-1]; v1_post_transfert[:]]
        v2 = [v2_pre_transfert[1:end-1]; v2_transfert[1:end-1]; v2_post_transfert[:]]
        θ  = [ θ_pre_transfert[1:end-1];  θ_transfert[1:end-1];  θ_post_transfert[:]]
        F  = [ F_pre_transfert[1:end-1];  F_transfert[1:end-1];  F_post_transfert[:]]
    
        # plot thrust on/off
        th = [BitArray(zeros(n_pre_transfert-1)); 
            BitArray(ones(n_transfert-1));
            BitArray(zeros(n_post_transfert))]
        
        # plot trajectory
        pt = [BitArray(ones(n_pre_transfert-1)); 
            BitArray(ones(n_transfert-1));
            BitArray(zeros(n_post_transfert))]
        
        # Contrôle sur la trajectoire totale
        u_total = hcat([zeros(2, n_pre_transfert-1), 
            u_transfert[:, 1:n_transfert-1],
            zeros(2, n_post_transfert)]...)
    
        # temps total
        temps_transfert_global = times[end]
    
        # pas de temps pour le transfert global
        if nFrame>1
            Δt = temps_transfert_global/(nFrame-1)
        else
            Δt = 0.0
        end
        
        # opacités des orbites initiale et finale
        op_initi = [range(0.0, 1.0, length=n_pre_transfert-1);
                    range(1.0, 0.0, length=n_transfert-1);
                    zeros(n_post_transfert)]
        op_final = [zeros(n_pre_transfert-1);
                    range(0.0, 1.0, length=n_transfert-1);
                    range(1.0, 0.0, length=int(n_post_transfert/4));
                    zeros(n_post_transfert-int(n_post_transfert/4))]
        
        println("Fps     = ", fps)
        println("NFrame  = ", nFrame)
        println("Vitesse = ", nFrame/(temps_transfert_global*fps)) 
        println("Durée totale de la mission = ", temps_transfert_global, " h") 
        
        # animation
        anim = @animate for i ∈ 1:nFrame
            
            # Δt : pas de temps
            # time_current : temps courant de la mission totale à l'itération i
            # i_current : indice tel que times[i_current] = time_current
            # w, h : width, height de la fenêtre
            # xmin, xmax, ymin, ymax : limites des axes du plot principal
            # X1_orb_init, X2_orb_init : coordonnées de l'orbite initiale
            # X1_orb_arr, X2_orb_arr :  coordonnées de l'orbite finale
            # cx, cy : coordonées du centre de l'affichage du tranfert
            # S : data pour les étoiles
            # Δx, Δy : xmax-xmin, ymax-ymin
            # times : tous les temps de la mission complète, ie pre-transfert, transfert et post-transfert
            # x1, x2 : vecteur de positions du satellite
            # θ : vecteur d'orientations du satellite
            # th : vecteur de booléens - thrust on/off
            # u_total : vecteur de contrôles pour toute la mission
            # F_max, γ_max : poussée max
            # subplot_current : valeur du subplot courant
            # cam_x, cam_y : position de la caméra
            # cam_zoom : zoom de la caméra
            
            cam_x    = cx
            cam_y    = cy
            cam_zoom = 1
            
            time_current = (i-1)*Δt
            i_current = argmin(abs.(times.-time_current))
            
            px = background(w, h, xmin, xmax, ymin, ymax, 
            X1_orb_init, X2_orb_init, X1_orb_arr, X2_orb_arr,
            cx, cy, S, Δx, Δy, cam_x, cam_y, cam_zoom, 
            op_initi[i_current], op_final[i_current], times, time_current)
    
            trajectoire!(px, times, x1, x2, θ, F, th, time_current, cx, cy, pt)        
        
            subplot_current = 2
            subplot_current = panneau_control!(px, time_current, times, u_total, 
                F_max, subplot_current)
            
            subplot_current = panneau_information!(px, subplot_current, time_current, 
                i_current, x1, x2, v1, v2, θ, F_max, tf_transfert, X1_orb_init, 
                X2_orb_init, X1_orb_arr, X2_orb_arr)
            
        end
    
        # enregistrement
        #gif(anim, "transfert-temps-min-original.mp4", fps=fps);
        gif(anim, "transfert-temps-min.gif", fps=fps);
        
    end;

    # --------------------------------------------------------------------------------------------
    # Preliminaries
    int(x) = floor(Int, x);
    rayon_Terre = 6371;
    sc_sat = 1000; # scaling for the satellite

    # --------------------------------------------------------------------------------------------
    #
    # Satellite
    #
    # --------------------------------------------------------------------------------------------
    # Corps du satellite
    @userplot SatBody
    @recipe function f(cp::SatBody)
        x, y = cp.args
        seriestype --> :shape
        fillcolor  --> :goldenrod1
        linecolor  --> :goldenrod1
        x, y
    end

    @userplot SatLine
    @recipe function f(cp::SatLine)
        x, y = cp.args
        linecolor  --> :black
        x, y
    end

    # Bras du satellite
    @userplot SatBras
    @recipe function f(cp::SatBras)
        x, y = cp.args
        seriestype --> :shape
        fillcolor  --> :goldenrod1
        linecolor  --> :white
        x, y
    end

    # panneau
    @userplot PanBody
    @recipe function f(cp::PanBody)
        x, y = cp.args
        seriestype --> :shape
        fillcolor  --> :dodgerblue4
        linecolor  --> :black
        x, y
    end

    # flamme
    @userplot Flamme
    @recipe function f(cp::Flamme)
        x, y = cp.args
        seriestype --> :shape
        fillcolor  --> :darkorange1
        linecolor  --> false
        x, y
    end

    function satellite!(pl; subplot=1, thrust=false, position=[0;0], scale=1, rotate=0, thrust_val=1.0)
        
        # Fonctions utiles
        R(θ) = [ cos(θ) -sin(θ)
                sin(θ)  cos(θ)] # rotation
        T(x, v) = x.+v # translation
        H(λ, x) = λ.*x # homotéthie
        SA(x, θ, c) = c .+ 2*[cos(θ);sin(θ)]'*(x.-c).*[cos(θ);sin(θ)]-(x.-c) # symétrie axiale
        
        #
        O  = position
        Rθ = R(rotate)
        
        # Paramètres
        α  = π/10.0     # angle du tube, par rapport à l'horizontal
        β  = π/2-π/40   # angle des bras, par rapport à l'horizontal
        Lp = scale.*1.4*cos(α) # longueur d'un panneau
        lp = scale.*2.6*sin(α) # largueur d'un panneau
        
        # Param bras du satellite
        lb = scale.*3*cos(α)   # longueur des bras
        eb = scale.*cos(α)/30  # demi largeur des bras
        xb = 0.0
        yb = scale.*sin(α)

        # Paramètres corps du satellite
        t  = range(-α, α, length=50)
        x  = scale.*cos.(t);
        y  = scale.*sin.(t);
        Δx = scale.*cos(α)
        Δy = scale.*sin(α)
        
        # Paramètres flamme
        hF = yb # petite hauteur
        HF = 2*hF # grande hauteur
        LF = 5.5*Δx # longueur
        
        # Dessin bras du satellite
        M = T(Rθ*[ [xb-eb, xb+eb, xb+eb+lb*cos(β), xb-eb+lb*cos(β), xb-eb]';
                [yb, yb, yb+lb*sin(β), yb+lb*sin(β), yb]'], O); 
        satbras!(pl, M[1,:], M[2,:], subplot=subplot)
        M = T(Rθ*[ [xb-eb, xb+eb, xb+eb+lb*cos(β), xb-eb+lb*cos(β), xb-eb]';
                -[yb, yb, yb+lb*sin(β), yb+lb*sin(β), yb']'], O); 
        satbras!(pl, M[1,:], M[2,:], subplot=subplot)
        
        # Dessin corps du satellite
        M = T(Rθ*[ x'; y'], O); satbody!(pl, M[1,:], M[2,:], subplot=subplot)  # bord droit
        M = T(Rθ*[-x'; y'], O); satbody!(pl, M[1,:], M[2,:], subplot=subplot)  # bord gauche
        M = T(Rθ*[scale.*[-cos(α), cos(α), cos(α), -cos(α)]'
                scale.*[-sin(α), -sin(α), sin(α), sin(α)]'], O); 
        satbody!(pl, M[1,:], M[2,:], subplot=subplot) # interieur

        M = T(Rθ*[ x'; y'], O); satline!(pl, M[1,:], M[2,:], subplot=subplot)  # bord droit
        M = T(Rθ*[-x'; y'], O); satline!(pl, M[1,:], M[2,:], subplot=subplot)  # bord gauche
        M = T(Rθ*[ x'.-2*Δx; y'], O); satline!(pl, M[1,:], M[2,:], subplot=subplot) # bord gauche (droite)
        M = T(Rθ*[ scale.*[-cos(α), cos(α)]'; 
                scale.*[ sin(α), sin(α)]'], O);
        satline!(pl, M[1,:], M[2,:], subplot=subplot) # haut
        M = T(Rθ*[  scale.*[-cos(α), cos(α)]'; 
                -scale.*[ sin(α), sin(α)]'], O);
        satline!(pl, M[1,:], M[2,:], subplot=subplot) # bas

        # Panneau
        ep = (lb-3*lp)/6
        panneau = [0 Lp Lp 0
                0 0 lp lp]

        ey = 3*eb # eloignement des panneaux au bras
        vy = [cos(β-π/2); sin(β-π/2)] .* ey
        v0 = [0; yb]
        v1 = 2*ep*[cos(β); sin(β)]
        v2 = (3*ep+lp)*[cos(β); sin(β)]
        v3 = (4*ep+2*lp)*[cos(β); sin(β)]

        pa1 = T(R(β-π/2)*panneau, v0+v1+vy); pa = T(Rθ*pa1, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa2 = T(R(β-π/2)*panneau, v0+v2+vy); pa = T(Rθ*pa2, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa3 = T(R(β-π/2)*panneau, v0+v3+vy); pa = T(Rθ*pa3, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)

        pa4 = SA(pa1, β, [xb; yb]); pa = T(Rθ*pa4, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa5 = SA(pa2, β, [xb; yb]); pa = T(Rθ*pa5, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa6 = SA(pa3, β, [xb; yb]); pa = T(Rθ*pa6, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)

        pa7 = SA(pa1, 0, [0; 0]); pa = T(Rθ*pa7, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa8 = SA(pa2, 0, [0; 0]); pa = T(Rθ*pa8, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa9 = SA(pa3, 0, [0; 0]); pa = T(Rθ*pa9, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)

        pa10 = SA(pa7, -β, [xb; -yb]); pa = T(Rθ*pa10, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa11 = SA(pa8, -β, [xb; -yb]); pa = T(Rθ*pa11, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)
        pa12 = SA(pa9, -β, [xb; -yb]); pa = T(Rθ*pa12, O); panbody!(pl, pa[1,:], pa[2,:], subplot=subplot)

        # Dessin flamme
        if thrust
            lthrust_max = 8000.0
            origin  = T(Rθ*[Δx, 0.0], O)
            thrust_ = thrust_val*Rθ*[lthrust_max, 0.0]
            quiver!(pl, [origin[1]], [origin[2]], color=:red, 
                    quiver=([thrust_[1]], [thrust_[2]]), linewidth=1, subplot=subplot)
        end
        
    end;

    # --------------------------------------------------------------------------------------------
    #
    # Stars
    #
    # --------------------------------------------------------------------------------------------
    @userplot StarsPlot
    @recipe function f(cp::StarsPlot)
        xo, yo, Δx, Δy, I, A, m, n = cp.args
        seriestype --> :scatter
        seriescolor  --> :white
        seriesalpha --> A
        markerstrokewidth --> 0
        markersize --> 2*I[3]
        xo.+I[2].*Δx./n, yo.+I[1].*Δy./m
    end

    mutable struct Stars
        s
        i
        m
        n
        a
    end

    # Etoiles
    function stars(ρ)
        n = 200       # nombre de colonnes
        m = int(ρ*n) # nombre de lignes
        d = 0.03
        s = sprand(m, n, d)
        i = findnz(s)
        s = dropzeros(s)
        # alpha
        amin = 0.6
        amax = 1.0
        Δa = amax-amin
        a  = amin.+rand(length(i[3])).*Δa
        return Stars(s, i, m, n, a)
    end;

    # --------------------------------------------------------------------------------------------
    #
    # Trajectory
    #
    # --------------------------------------------------------------------------------------------
    @userplot TrajectoryPlot
    @recipe function f(cp::TrajectoryPlot)
        t, x, y, tf = cp.args
        n = argmin(abs.(t.-tf))
        inds = 1:n
        seriescolor  --> :white
        linewidth --> range(0.0, 3.0, length=n)
        seriesalpha --> range(0.0, 1.0, length=n)
        aspect_ratio --> 1
        label --> false
        x[inds], y[inds]
    end

    function trajectoire!(px, times, x1, x2, θ, F, thrust, t_current, cx, cy, pt)
        
        i_current = argmin(abs.(times.-t_current))
        
        # Trajectoire 
        if t_current>0 && pt[i_current]
            trajectoryplot!(px, times, cx.+x1, cy.+x2, t_current)
        end
        
        # Satellite
        satellite!(px, position=[cx+x1[i_current]; cy+x2[i_current]], scale=sc_sat, 
            rotate=θ[i_current], thrust=thrust[i_current], thrust_val=F[i_current])
        
    end;

    # --------------------------------------------------------------------------------------------
    #
    # Background
    #
    # --------------------------------------------------------------------------------------------
    # Décor
    function background(w, h, xmin, xmax, ymin, ymax, 
            X1_orb_init, X2_orb_init, X1_orb_arr, X2_orb_arr,
            cx, cy, S, Δx, Δy, cam_x, cam_y, cam_zoom, opi, opf, times, time_current)

        # Fond
        px = plot(background_color=:gray8, legend = false, aspect_ratio=:equal, 
                size = (w, h), framestyle = :none, 
                left_margin=-25mm, bottom_margin=-10mm, top_margin=-10mm, right_margin=-10mm,
                xlims=(xmin, xmax), ylims=(ymin, ymax) #, 
                #camera=(0, 90), xlabel = "x", ylabel= "y", zlabel = "z", right_margin=25mm
        )
        
        # Etoiles
        starsplot!(px, xmin, ymin, Δx, Δy, S.i, S.a, S.m, S.n)
        
        starsplot!(px, xmin-Δx, ymin-Δy, Δx, Δy, S.i, S.a, S.m, S.n)
        starsplot!(px, xmin, ymin-Δy, Δx, Δy, S.i, S.a, S.m, S.n)
        starsplot!(px, xmin, ymin+Δy, Δx, Δy, S.i, S.a, S.m, S.n)
        starsplot!(px, xmin-Δx, ymin, Δx, Δy, S.i, S.a, S.m, S.n)
        starsplot!(px, xmin+Δx, ymin, Δx, Δy, S.i, S.a, S.m, S.n)
        starsplot!(px, xmin-Δx, ymin+Δy, Δx, Δy, S.i, S.a, S.m, S.n)
        starsplot!(px, xmin, ymin+Δy, Δx, Δy, S.i, S.a, S.m, S.n)
        starsplot!(px, xmin+Δx, ymin+Δy, Δx, Δy, S.i, S.a, S.m, S.n)

        # Orbite initiale
        nn = length(X2_orb_init)
        plot!(px, cx.+X1_orb_init, cy.+X2_orb_init, color = :olivedrab1, linewidth=2, alpha=opi)
        
        # Orbite finale
        nn = length(X2_orb_init)
        plot!(px, cx.+X1_orb_arr, cy.+X2_orb_arr, color = :turquoise1, linewidth=2, alpha=opf)
        
        # Terre
        θ = range(0.0, 2π, length=100)
        rT = rayon_Terre #- 1000
        xT = cx .+ rT .* cos.(θ)
        yT = cy .+ rT .* sin.(θ)
        nn = length(xT)
        plot!(px, xT, yT, color = :dodgerblue1, seriestype=:shape, linewidth=0)    
        
        # Soleil
        i_current = argmin(abs.(times.-time_current))
        e = π/6
        β = range(π/4+e, π/4-e, length=length(times))
        ρ = sqrt((0.8*xmax-cx)^2+(0.8*ymax-cy)^2)
        cxS = cx + ρ * cos(β[i_current])
        cyS = cy + ρ * sin(β[i_current])    
        θ = range(0.0, 2π, length=100)
        rS = 2000
        xS = cxS .+ rS .* cos.(θ)
        yS = cyS .+ rS .* sin.(θ)
        plot!(px, xS, yS, color = :gold, seriestype=:shape, linewidth=0)    
        
        # Point de départ
        #plot!(px, [cx+x0[1]], [cy+x0[2]], seriestype=:scatter, color = :white, markerstrokewidth=0)  
        
        # Cadre
        #plot!(px, [xmin, xmax, xmax, xmin, xmin], [ymin, ymin, ymax, ymax, ymin], color=:white)

        # Angle
        #plot!(px, camera=(30,90))
        
        return px
        
    end;

    function panneau_control!(px, time_current, times, u, F_max, subplot_current)
        
        # We set the (optional) position relative to top-left of the 1st subplot.
        # The call is `bbox(x, y, width, height, origin...)`,
        # where numbers are treated as "percent of parent".
        Δt_control = 2 #0.1*times[end]

        xx = 0.08
        yy = 0.1
        plot!(px, times, F_max.*u[1,:],
            inset = (1, bbox(xx, yy, 0.2, 0.15, :top, :left)),
            subplot = subplot_current,
            bg_inside = nothing,
            color=:red, legend=:true, yguidefontrotation=-90, yguidefonthalign = :right,
            xguidefontsize=14, legendfontsize=14,
            ylims=(-F_max,F_max), 
            xlims=(time_current-Δt_control, time_current+1.0*Δt_control),
            ylabel="u₁ [N]", label=""
        )

        plot!(px, 
            [time_current, time_current], [-F_max,F_max], 
            subplot = subplot_current, label="")

        plot!(px, times, F_max.*u[2,:],
            inset = (1, bbox(xx, yy+0.2, 0.2, 0.15, :top, :left)),
            #ticks = nothing,
            subplot = subplot_current+1,
            bg_inside = nothing,
            color=:red, legend=:true, yguidefontrotation=-90, yguidefonthalign = :right,
            xguidefontsize=14, legendfontsize=14,
            ylims=(-F_max,F_max), 
            xlims=(time_current-Δt_control, time_current+1.0*Δt_control),
            ylabel="u₂ [N]", xlabel="temps [h]", label=""
        )

        plot!(px, 
            [time_current, time_current], [-F_max,F_max], 
            subplot = subplot_current+1, label="")
        
        return subplot_current+2
    end;

    @enum PB tfmin=1 consomin=2 

    function panneau_information!(px, subplot_current, time_current, i_current, 
            x1, x2, v1, v2, θ, F_max, tf_transfert, X1_orb_init, X2_orb_init, X1_orb_arr, X2_orb_arr;
            pb=tfmin, tf_min=0.0, conso=[])
        
        # panneaux information
        xx = 0.06
        yy = 0.3
        plot!(px,
            inset = (1, bbox(xx, yy, 0.2, 0.15, :bottom, :left)),
            subplot=subplot_current, 
            bg_inside = nothing, legend=:false,
            framestyle=:none
        ) 

        a = 0.0
        b = 0.0
        Δtxt = 0.3

        txt = @sprintf("Temps depuis départ [h]  = %3.2f", time_current)
        plot!(px, subplot = subplot_current, 
            annotation=((a, b+3*Δtxt, txt)), 
            annotationcolor=:white, annotationfontsize=14,
            annotationhalign=:left)

        txt = @sprintf("Vitesse satellite [km/h]    = %5.2f", sqrt(v1[i_current]^2+v2[i_current]^2))
        plot!(px, subplot = subplot_current, 
            annotation=((a, b+2*Δtxt, txt)), 
            annotationcolor=:white, annotationfontsize=14,
            annotationhalign=:left)

        txt = @sprintf("Distance à la Terre [km]   = %5.2f", sqrt(x1[i_current]^2+x2[i_current]^2)-rayon_Terre)
        plot!(px, subplot = subplot_current, 
            annotation=((a, b+Δtxt, txt)), 
            annotationcolor=:white, annotationfontsize=14,
            annotationhalign=:left)

        txt = @sprintf("Poussée maximale [N]     = %5.2f", F_max)
        plot!(px, subplot = subplot_current, 
            annotation=((a, b-0*Δtxt, txt)), 
            annotationcolor=:white, annotationfontsize=14,
            annotationhalign=:left)

        txt = @sprintf("Durée du transfert [h]     = %5.2f", tf_transfert)
        plot!(px, subplot = subplot_current, 
            annotation=((a, b-1*Δtxt, txt)), 
            annotationcolor=:white, annotationfontsize=14,
            annotationhalign=:left)

        if pb==consomin
            txt = @sprintf("Durée et consommation comparées")
            plot!(px, subplot = subplot_current, 
                annotation=((a, b-2*Δtxt, txt)), 
                annotationcolor=:green, annotationfontsize=16,
                annotationhalign=:left)

            txt = @sprintf("au problème en temps minimal :")
            plot!(px, subplot = subplot_current, 
                annotation=((a, b-2.75*Δtxt, txt)), 
                annotationcolor=:green, annotationfontsize=16,
                annotationhalign=:left)

            txt = @sprintf("Durée du transfert [relatif] = %5.2f", tf_transfert/tf_min)
            plot!(px, subplot = subplot_current, 
                annotation=((a, b-3.75*Δtxt, txt)), 
                annotationcolor=:white, annotationfontsize=14,
                annotationhalign=:left)

            txt = @sprintf("Consommation [relative]   = %5.2f", conso[i_current]./tf_min)
            plot!(px, subplot = subplot_current, 
                annotation=((a, b-4.75*Δtxt, txt)), 
                annotationcolor=:white, annotationfontsize=14,
                annotationhalign=:left)
        end
        
        subplot_current = subplot_current+1
        plot!(px,
            inset = (1, bbox(0.3, 0.03, 0.5, 0.1, :top, :left)),
            subplot = subplot_current, 
            bg_inside = nothing, legend=:false, aspect_ratio=:equal,
            xlims=(-4, 4),
            framestyle=:none
        ) 

        rmax = 3*maximum(sqrt.(X1_orb_init.^2+X2_orb_init.^2))
        plot!(px, subplot = subplot_current, 
            0.04.+X1_orb_init./rmax, X2_orb_init./rmax.-0.03, 
            color = :olivedrab1, linewidth=2)

        rmax = 7*maximum(sqrt.(X1_orb_arr.^2+X2_orb_arr.^2))
        plot!(px, subplot = subplot_current, 
            3.3.+X1_orb_arr./rmax, X2_orb_arr./rmax.-0.03, 
            color = :turquoise1, linewidth=2)    
        
        satellite!(px, subplot=subplot_current,
            position=[0.67; 0.28], scale=0.08, 
            rotate=π/2)
        
        # Terre
        θ = range(0.0, 2π, length=100)
        rT = 0.1
        xT = 3.55 .+ rT .* cos.(θ)
        yT = 0.28 .+ rT .* sin.(θ)
        plot!(px, subplot=subplot_current, xT, yT, color = :dodgerblue1, seriestype=:shape, linewidth=0) 
        
        s1 = "Transfert orbital du satellite        autour de la Terre\n"
        s2 = "de l'orbite elliptique       vers l'orbite circulaire\n"
        if pb==tfmin
            s3 = "en temps minimal"
        elseif pb==consomin
            s3 = "en consommation minimale"        
        else
            error("pb pb")
        end
        txt = s1 * s2 * s3
        plot!(px, subplot = subplot_current, 
            annotation=((0, 0, txt)), 
            annotationcolor=:white, annotationfontsize=18,
            annotationhalign=:center)
        
        # Realisation
        subplot_current = subplot_current+1
        xx = 0.0
        yy = 0.02
        plot!(px,
            inset = (1, bbox(xx, yy, 0.12, 0.05, :bottom, :right)),
            subplot = subplot_current, 
            bg_inside = nothing, legend=:false,
            framestyle=:none
        ) 

        s1 = "Réalisation : Olivier Cots (2022)"
        txt = s1
        plot!(px, subplot = subplot_current, 
            annotation=((0, 0, txt)), 
            annotationcolor=:gray, annotationfontsize=8, annotationhalign=:left)
        
        return subplot_current+1

    end;

end # module