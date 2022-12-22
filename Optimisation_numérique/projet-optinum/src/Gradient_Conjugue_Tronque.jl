@doc doc"""
#### Objet
Cette fonction calcule une solution approchée du problème

```math
\min_{||s||< \Delta}  q(s) = s^{t} g + \frac{1}{2} s^{t}Hs
```

par l'algorithme du gradient conjugué tronqué

#### Syntaxe
```julia
s = Gradient_Conjugue_Tronque(g,H,option)
```

#### Entrées :   
   - g : (Array{Float,1}) un vecteur de ``\mathbb{R}^n``
   - H : (Array{Float,2}) une matrice symétrique de ``\mathbb{R}^{n\times n}``
   - options          : (Array{Float,1})
      - delta    : le rayon de la région de confiance
      - max_iter : le nombre maximal d'iterations
      - tol      : la tolérance pour la condition d'arrêt sur le gradient

#### Sorties:
   - s : (Array{Float,1}) le pas s qui approche la solution du problème : ``min_{||s||< \Delta} q(s)``

#### Exemple d'appel:
```julia
gradf(x)=[-400*x[1]*(x[2]-x[1]^2)-2*(1-x[1]) ; 200*(x[2]-x[1]^2)]
hessf(x)=[-400*(x[2]-3*x[1]^2)+2  -400*x[1];-400*x[1]  200]
xk = [1; 0]
options = []
s = Gradient_Conjugue_Tronque(gradf(xk),hessf(xk),options)
```
"""
function Gradient_Conjugue_Tronque(g,H,options)

    "# Si option est vide on initialise les 3 paramètres par défaut"
    if options == []
        delta = 2
        max_iter = 100
        tol = 1e-6
    else
        delta = options[1]
        max_iter = options[2]
        tol = options[3]
    end

   
    n = length(g)
    s = zeros(n)
    sj=s
    iter=0
    q(s)=g'*s +(1/2)*s'*H*s
    j=0
    pj=-g
    gj=g
    while ((j<2*n && norm(gj)>max(tol,(norm(g)*tol))))
     kj=pj' * H *pj
      if (kj<=0)
        #compute σk as the positive root of ksk + σpk k2 = ∆k
        a=norm(pj)^2
        b= 2*sj'*pj
        c= norm(sj)^2 - delta^2
        D= sqrt(b^2 - 4*a*c)
        sigma1 = (-b+D)/(2*a)
        sigma2 =(-b-D)/(2*a)
        if q(sigma1*pj+sj)>q(sigma2*pj+sj)
             sigmaj=sigma2
        else 
            sigmaj=sigma1
        end     
        s = sj+sigmaj*pj 
        break
      end 
 
      alphaj=(gj'*gj)/kj
      if  norm(sj+alphaj*pj)>=delta
        #compute σk as the positive root of ksk + σpk k2 = ∆k
        a=norm(pj)^2
        b= 2*sj'*pj
        c= norm(sj)^2 - delta^2
        D=sqrt(b^2 -4*a*c)
        sigma1 = (-b+D)/(2*a)
        sigma2 =(-b-D)/(2*a)
       if sigma1>0
             sigmaj=sigma1
        elseif sigma2>0
             sigmaj=sigma2          
        end
        s=sj+sigmaj*pj
        break  
        
      end
    iter+=1 
    sj=sj+alphaj*pj
    gj_1=gj+alphaj*H*pj
    betaj=(gj_1'*gj_1)/(gj'*gj)
    pj_1= -gj_1 + betaj*pj
    gj=gj_1
    pj=pj_1
    s=sj
    j+=1
     
    if iter==max_iter
      break
     end
     
  end
  return s
end
