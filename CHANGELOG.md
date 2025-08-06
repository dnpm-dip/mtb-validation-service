# Changelog

## 1.0.0 (2025-08-06)


### Features

* Adapted validators and tests to updated model: Upgraded to Scala 2.13.16 ([22f1f9b](https://github.com/dnpm-dip/mtb-validation-service/commit/22f1f9b29f0fa25799830c38d25c263c25856421))
* Added check for presence of therapy documentations when follow-ups are declared ([e9803ca](https://github.com/dnpm-dip/mtb-validation-service/commit/e9803ca701de3485598ecee435cc198a5c5b971e))
* Added further validations to MTBPatientRecord; Adapted Tests to refactored base ([1f7d99e](https://github.com/dnpm-dip/mtb-validation-service/commit/1f7d99eabd85156ba77e69ad387a1b0d56e0b16b))
* Added regex-based (i.e. plausibility) validation of TNM classification codes ([869fe8a](https://github.com/dnpm-dip/mtb-validation-service/commit/869fe8aea5a32150ded4f24223827cde6c0323ed))
* Added validator for MSI finding ([979c662](https://github.com/dnpm-dip/mtb-validation-service/commit/979c662582b767bc19c507d6b3885ba204ef4556))
* Aligned validation rules with documentation ([23f6112](https://github.com/dnpm-dip/mtb-validation-service/commit/23f6112d54f27687998477d4c0f5b7da8a166a9f))


### Bug Fixes

* Adapted MTBCarePlan validator to refactored model object ([3b7647f](https://github.com/dnpm-dip/mtb-validation-service/commit/3b7647fcf0f6189c3bb90fc6259da5771d8de8f3))
* Adapted scalac linting and fixed many reported errors (mostly unused imports) ([b8ef64b](https://github.com/dnpm-dip/mtb-validation-service/commit/b8ef64bf0d50ca83490a340dc13b6b2e06dd046b))
* Adapted validation rules; Fixed bug with reference resolution in OncoProcedure validation ([c9778df](https://github.com/dnpm-dip/mtb-validation-service/commit/c9778dfa92d29b5a95f168dfc845ee378a3b79c2))
* Added forgotten changes ([236d6f0](https://github.com/dnpm-dip/mtb-validation-service/commit/236d6f087a6cd91a018514e414558beec477c7a5))
* Clean-up ([efc6674](https://github.com/dnpm-dip/mtb-validation-service/commit/efc66747328777bab325f1a8358837f6512f2d98))
* Corrected TNM validation regexes ([176f0c6](https://github.com/dnpm-dip/mtb-validation-service/commit/176f0c6b1b1d8b87eef3fa4c5a93abbad96ccd36))
* Correction to TNM-code regexes ([356f42c](https://github.com/dnpm-dip/mtb-validation-service/commit/356f42c11f1713e93267119538e6914f63eaaac0))
* Factored out TNM validation regexes and added tests for them ([6a1cdbc](https://github.com/dnpm-dip/mtb-validation-service/commit/6a1cdbc30fd325a9cb08cb370cb47cdc64c82460))
* Fixes/imrpvements to validators ([50d9a5a](https://github.com/dnpm-dip/mtb-validation-service/commit/50d9a5a5e8f14655db6bb5f866e6a6637188ab21))
* Improved/corrected RegExes for TNM code validation ([c0af16c](https://github.com/dnpm-dip/mtb-validation-service/commit/c0af16c1ebe8a878aa8d19c7e54e3ab5519426cd))
