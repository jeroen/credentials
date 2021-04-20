test_that("parsing github_pat urls", {
  # The default
  out <- get_pat_credential_url('https://api.github.com')
  expect_equal(out$url, 'https://PersonalAccessToken@github.com')
  expect_equal(out$var, 'GITHUB_PAT')

  # Custom enterprise server
  out <- get_pat_credential_url('https://github.ucla.eu/api/v3')
  expect_equal(out$url, 'https://PersonalAccessToken@github.ucla.eu')
  expect_equal(out$var, 'GITHUB_PAT_github_ucla_eu')

  # With a custom token 'key' (i.e. username)
  out <- get_pat_credential_url('https://dummy@api.github.com')
  expect_equal(out$url, 'https://dummy@github.com')
  expect_equal(out$var, 'GITHUB_PAT')

  # With a custom token 'key' and Server
  out <- get_pat_credential_url('https://dummy@github.ucla.eu/api/v3')
  expect_equal(out$url, 'https://dummy@github.ucla.eu')
  expect_equal(out$var, 'GITHUB_PAT_github_ucla_eu')
})
