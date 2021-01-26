test_that("affecte_une_classe returns a factor", {
			seuil <- makeSeuils(CdParametre="1335", type_seuil="DCE")[[1]]
			test <- sample(seq(0,0.6, 0.1),10, replace=T)
			res <- affecte_une_classe(test, seuil)
			expect_type(res, "integer")
			expect_that(res, is_a("factor") , label= "affecte_une_classe should return a factor")
})
