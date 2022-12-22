@doc doc"""
Tester l'algorithme du pas de cauchy

# Entrées :
   * afficher : (Bool) affichage ou non des résultats de chaque test

# Les cas de test (dans l'ordre)
   * la quadratique 1
   * la quadratique 2
   * la quadratique 3
   * la quadratique 4
   * la quadratique 5
   * la quadratique 6
"""
function tester_pas_de_cauchy(afficher::Bool,Pas_De_Cauchy::Function)

    tol = 1e-7
    max_iter = 100
    # Tolérance utilisé dans les tests
    tol_test = 1e-3

    @testset "Pas_De_Cauchy" begin
        # le cas de test 1
        grad = [0 ; 0]
        Hess = [7 0 ; 0 2]
        delta = 1
        s,e =Pas_De_Cauchy(grad,Hess,delta)
        @test  s ≈ [0.0 ; 0.0] atol = tol_test
         @test  e ≈ 0.0 atol = tol_test

        # le cas de test 2 H definie positive
        grad = [6 ; 2]
        Hess = [7 0 ; 0 2]
        delta = 0.5       # sol = pas de Cauchy  
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test  s ≈ -delta*grad/norm(grad) atol = tol_test
        @test  e ≈ -1 atol = tol_test   

        # le cas test 2 bis matrice avec 1 vp < 0 et 1 vp > 0
        grad = [1,2]
        Hess = [1 0 ; 0 -1]
        delta = 1.       # g^T H g < 0 première direction concave
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test  s ≈ -delta*grad/norm(grad) atol = tol_test
        @test  e ≈ 1 atol = tol_test
        
        

        # le cas de test 6
        # Le pas de Cauchy conduit à un gradient nul en 1 itération
        grad = [2 ; 0]
        Hess = [4 0 ; 0 -15]
        delta = 2
        s,e = Pas_De_Cauchy(grad,Hess,delta)
        @test  s ≈ [-0.5 ; 0.0] atol = tol_test
        @test  e ≈ 1 atol = tol_test
    end
end