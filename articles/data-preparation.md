# Data Preparation and Integration

## Overview

Before fitting any model in mlumr, you need to:

1.  Set up the IPD with
    [`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md)
2.  Set up the AgD with
    [`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
3.  Combine them with
    [`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md)
4.  (For ML-UMR only) Add integration points with
    [`add_integration()`](https://choxos.github.io/mlumr/reference/add_integration.md)

## Setting up IPD

IPD should be a data frame with one row per patient, containing:

- A treatment indicator column
- An outcome column (binary 0/1, continuous numeric, or non-negative
  integer counts depending on `family`)
- Covariate columns
- For Poisson family: an exposure column

``` r

library(mlumr)
set.seed(2026)

# Example: Trial A data (index treatment)
trial_a <- data.frame(
  patient_id = 1:500,
  treatment = "Drug_A",
  response = rbinom(500, 1, 0.6),
  age_group = rbinom(500, 1, 0.4),   # 0 = young, 1 = old
  sex = rbinom(500, 1, 0.55)          # 0 = male, 1 = female
)

ipd <- set_ipd(
  data = trial_a,
  treatment = "treatment",
  outcome = "response",
  covariates = c("age_group", "sex")
)

ipd
#> $data
#>        .study   .trt .outcome age_group sex
#> 1   IPD_Study Drug_A        0         0   0
#> 2   IPD_Study Drug_A        1         1   0
#> 3   IPD_Study Drug_A        1         1   0
#> 4   IPD_Study Drug_A        1         0   1
#> 5   IPD_Study Drug_A        1         0   1
#> 6   IPD_Study Drug_A        1         0   0
#> 7   IPD_Study Drug_A        1         0   0
#> 8   IPD_Study Drug_A        0         1   1
#> 9   IPD_Study Drug_A        1         1   1
#> 10  IPD_Study Drug_A        1         0   1
#> 11  IPD_Study Drug_A        1         0   1
#> 12  IPD_Study Drug_A        0         0   0
#> 13  IPD_Study Drug_A        1         0   0
#> 14  IPD_Study Drug_A        0         0   1
#> 15  IPD_Study Drug_A        1         1   1
#> 16  IPD_Study Drug_A        1         0   0
#> 17  IPD_Study Drug_A        1         0   1
#> 18  IPD_Study Drug_A        1         1   0
#> 19  IPD_Study Drug_A        1         0   0
#> 20  IPD_Study Drug_A        1         0   1
#> 21  IPD_Study Drug_A        1         1   0
#> 22  IPD_Study Drug_A        1         1   1
#> 23  IPD_Study Drug_A        1         1   0
#> 24  IPD_Study Drug_A        1         1   0
#> 25  IPD_Study Drug_A        1         0   1
#> 26  IPD_Study Drug_A        1         1   0
#> 27  IPD_Study Drug_A        1         0   1
#> 28  IPD_Study Drug_A        1         0   0
#> 29  IPD_Study Drug_A        1         0   0
#> 30  IPD_Study Drug_A        0         0   0
#> 31  IPD_Study Drug_A        0         0   1
#> 32  IPD_Study Drug_A        1         0   1
#> 33  IPD_Study Drug_A        0         0   1
#> 34  IPD_Study Drug_A        1         0   1
#> 35  IPD_Study Drug_A        1         1   0
#> 36  IPD_Study Drug_A        1         0   0
#> 37  IPD_Study Drug_A        1         1   1
#> 38  IPD_Study Drug_A        1         0   1
#> 39  IPD_Study Drug_A        0         0   1
#> 40  IPD_Study Drug_A        1         0   1
#> 41  IPD_Study Drug_A        1         1   1
#> 42  IPD_Study Drug_A        0         1   1
#> 43  IPD_Study Drug_A        1         0   0
#> 44  IPD_Study Drug_A        1         1   1
#> 45  IPD_Study Drug_A        1         0   1
#> 46  IPD_Study Drug_A        1         0   0
#> 47  IPD_Study Drug_A        0         1   1
#> 48  IPD_Study Drug_A        1         0   1
#> 49  IPD_Study Drug_A        1         1   1
#> 50  IPD_Study Drug_A        1         0   0
#> 51  IPD_Study Drug_A        0         1   0
#> 52  IPD_Study Drug_A        0         0   0
#> 53  IPD_Study Drug_A        0         1   1
#> 54  IPD_Study Drug_A        0         0   1
#> 55  IPD_Study Drug_A        1         0   0
#> 56  IPD_Study Drug_A        0         0   1
#> 57  IPD_Study Drug_A        0         0   1
#> 58  IPD_Study Drug_A        1         1   1
#> 59  IPD_Study Drug_A        0         0   0
#> 60  IPD_Study Drug_A        1         0   0
#> 61  IPD_Study Drug_A        1         1   0
#> 62  IPD_Study Drug_A        0         1   1
#> 63  IPD_Study Drug_A        1         1   1
#> 64  IPD_Study Drug_A        1         0   1
#> 65  IPD_Study Drug_A        1         0   0
#> 66  IPD_Study Drug_A        1         0   1
#> 67  IPD_Study Drug_A        0         0   1
#> 68  IPD_Study Drug_A        1         0   1
#> 69  IPD_Study Drug_A        1         1   0
#> 70  IPD_Study Drug_A        0         0   1
#> 71  IPD_Study Drug_A        0         0   0
#> 72  IPD_Study Drug_A        1         1   0
#> 73  IPD_Study Drug_A        0         0   0
#> 74  IPD_Study Drug_A        0         0   0
#> 75  IPD_Study Drug_A        1         0   0
#> 76  IPD_Study Drug_A        0         0   1
#> 77  IPD_Study Drug_A        0         1   1
#> 78  IPD_Study Drug_A        1         0   1
#> 79  IPD_Study Drug_A        1         0   0
#> 80  IPD_Study Drug_A        0         1   0
#> 81  IPD_Study Drug_A        1         1   0
#> 82  IPD_Study Drug_A        0         1   0
#> 83  IPD_Study Drug_A        0         0   0
#> 84  IPD_Study Drug_A        0         0   1
#> 85  IPD_Study Drug_A        1         1   0
#> 86  IPD_Study Drug_A        0         0   0
#> 87  IPD_Study Drug_A        0         1   0
#> 88  IPD_Study Drug_A        0         1   0
#> 89  IPD_Study Drug_A        1         0   1
#> 90  IPD_Study Drug_A        1         0   0
#> 91  IPD_Study Drug_A        0         1   1
#> 92  IPD_Study Drug_A        0         0   0
#> 93  IPD_Study Drug_A        0         0   1
#> 94  IPD_Study Drug_A        1         0   1
#> 95  IPD_Study Drug_A        1         0   1
#> 96  IPD_Study Drug_A        1         0   1
#> 97  IPD_Study Drug_A        0         0   0
#> 98  IPD_Study Drug_A        1         1   1
#> 99  IPD_Study Drug_A        0         0   1
#> 100 IPD_Study Drug_A        1         1   0
#> 101 IPD_Study Drug_A        1         0   0
#> 102 IPD_Study Drug_A        0         1   1
#> 103 IPD_Study Drug_A        1         0   0
#> 104 IPD_Study Drug_A        1         1   1
#> 105 IPD_Study Drug_A        1         0   0
#> 106 IPD_Study Drug_A        1         0   1
#> 107 IPD_Study Drug_A        0         1   1
#> 108 IPD_Study Drug_A        0         1   0
#> 109 IPD_Study Drug_A        1         0   1
#> 110 IPD_Study Drug_A        1         1   1
#> 111 IPD_Study Drug_A        1         0   0
#> 112 IPD_Study Drug_A        0         0   0
#> 113 IPD_Study Drug_A        1         0   1
#> 114 IPD_Study Drug_A        1         1   0
#> 115 IPD_Study Drug_A        1         1   1
#> 116 IPD_Study Drug_A        1         1   0
#> 117 IPD_Study Drug_A        1         1   1
#> 118 IPD_Study Drug_A        1         0   1
#> 119 IPD_Study Drug_A        1         1   0
#> 120 IPD_Study Drug_A        0         0   1
#> 121 IPD_Study Drug_A        1         0   1
#> 122 IPD_Study Drug_A        1         0   1
#> 123 IPD_Study Drug_A        1         0   0
#> 124 IPD_Study Drug_A        0         1   0
#> 125 IPD_Study Drug_A        1         1   1
#> 126 IPD_Study Drug_A        1         0   0
#> 127 IPD_Study Drug_A        1         1   1
#> 128 IPD_Study Drug_A        1         1   0
#> 129 IPD_Study Drug_A        1         0   1
#> 130 IPD_Study Drug_A        1         1   0
#> 131 IPD_Study Drug_A        1         0   0
#> 132 IPD_Study Drug_A        0         0   0
#> 133 IPD_Study Drug_A        1         0   1
#> 134 IPD_Study Drug_A        1         1   1
#> 135 IPD_Study Drug_A        0         0   0
#> 136 IPD_Study Drug_A        1         1   1
#> 137 IPD_Study Drug_A        1         1   1
#> 138 IPD_Study Drug_A        1         1   1
#> 139 IPD_Study Drug_A        1         1   0
#> 140 IPD_Study Drug_A        1         0   1
#> 141 IPD_Study Drug_A        0         1   0
#> 142 IPD_Study Drug_A        1         1   0
#> 143 IPD_Study Drug_A        1         0   1
#> 144 IPD_Study Drug_A        0         0   0
#> 145 IPD_Study Drug_A        0         0   0
#> 146 IPD_Study Drug_A        1         1   0
#> 147 IPD_Study Drug_A        1         1   0
#> 148 IPD_Study Drug_A        0         0   0
#> 149 IPD_Study Drug_A        0         1   0
#> 150 IPD_Study Drug_A        1         0   1
#> 151 IPD_Study Drug_A        1         1   0
#> 152 IPD_Study Drug_A        1         1   1
#> 153 IPD_Study Drug_A        0         1   0
#> 154 IPD_Study Drug_A        1         0   1
#> 155 IPD_Study Drug_A        0         0   0
#> 156 IPD_Study Drug_A        0         0   0
#> 157 IPD_Study Drug_A        1         1   0
#> 158 IPD_Study Drug_A        0         0   1
#> 159 IPD_Study Drug_A        0         1   0
#> 160 IPD_Study Drug_A        1         1   1
#> 161 IPD_Study Drug_A        1         0   1
#> 162 IPD_Study Drug_A        1         0   0
#> 163 IPD_Study Drug_A        0         0   0
#> 164 IPD_Study Drug_A        1         0   0
#> 165 IPD_Study Drug_A        1         0   0
#> 166 IPD_Study Drug_A        1         0   0
#> 167 IPD_Study Drug_A        1         0   0
#> 168 IPD_Study Drug_A        1         1   1
#> 169 IPD_Study Drug_A        1         0   0
#> 170 IPD_Study Drug_A        1         1   0
#> 171 IPD_Study Drug_A        0         1   0
#> 172 IPD_Study Drug_A        0         1   0
#> 173 IPD_Study Drug_A        1         0   1
#> 174 IPD_Study Drug_A        0         0   0
#> 175 IPD_Study Drug_A        1         0   0
#> 176 IPD_Study Drug_A        1         1   1
#> 177 IPD_Study Drug_A        0         1   1
#> 178 IPD_Study Drug_A        0         1   1
#> 179 IPD_Study Drug_A        0         1   1
#> 180 IPD_Study Drug_A        1         1   1
#> 181 IPD_Study Drug_A        1         1   1
#> 182 IPD_Study Drug_A        1         0   0
#> 183 IPD_Study Drug_A        0         1   1
#> 184 IPD_Study Drug_A        0         0   1
#> 185 IPD_Study Drug_A        1         1   0
#> 186 IPD_Study Drug_A        1         0   1
#> 187 IPD_Study Drug_A        0         0   0
#> 188 IPD_Study Drug_A        1         0   1
#> 189 IPD_Study Drug_A        1         0   1
#> 190 IPD_Study Drug_A        1         0   1
#> 191 IPD_Study Drug_A        1         1   1
#> 192 IPD_Study Drug_A        1         0   0
#> 193 IPD_Study Drug_A        1         1   1
#> 194 IPD_Study Drug_A        0         0   1
#> 195 IPD_Study Drug_A        1         1   1
#> 196 IPD_Study Drug_A        1         1   1
#> 197 IPD_Study Drug_A        1         0   1
#> 198 IPD_Study Drug_A        1         0   0
#> 199 IPD_Study Drug_A        0         1   1
#> 200 IPD_Study Drug_A        1         0   1
#> 201 IPD_Study Drug_A        0         1   0
#> 202 IPD_Study Drug_A        0         0   1
#> 203 IPD_Study Drug_A        1         0   1
#> 204 IPD_Study Drug_A        0         0   1
#> 205 IPD_Study Drug_A        1         1   1
#> 206 IPD_Study Drug_A        0         0   0
#> 207 IPD_Study Drug_A        1         0   0
#> 208 IPD_Study Drug_A        0         1   0
#> 209 IPD_Study Drug_A        0         0   1
#> 210 IPD_Study Drug_A        1         0   1
#> 211 IPD_Study Drug_A        0         0   1
#> 212 IPD_Study Drug_A        1         1   0
#> 213 IPD_Study Drug_A        1         0   1
#> 214 IPD_Study Drug_A        1         0   1
#> 215 IPD_Study Drug_A        0         0   1
#> 216 IPD_Study Drug_A        1         1   1
#> 217 IPD_Study Drug_A        1         0   1
#> 218 IPD_Study Drug_A        1         0   0
#> 219 IPD_Study Drug_A        1         0   0
#> 220 IPD_Study Drug_A        1         1   1
#> 221 IPD_Study Drug_A        1         1   0
#> 222 IPD_Study Drug_A        0         0   1
#> 223 IPD_Study Drug_A        1         0   1
#> 224 IPD_Study Drug_A        0         0   1
#> 225 IPD_Study Drug_A        1         0   0
#> 226 IPD_Study Drug_A        0         1   1
#> 227 IPD_Study Drug_A        0         0   1
#> 228 IPD_Study Drug_A        1         0   0
#> 229 IPD_Study Drug_A        1         1   1
#> 230 IPD_Study Drug_A        1         0   1
#> 231 IPD_Study Drug_A        1         1   1
#> 232 IPD_Study Drug_A        1         0   0
#> 233 IPD_Study Drug_A        1         0   1
#> 234 IPD_Study Drug_A        1         0   0
#> 235 IPD_Study Drug_A        0         0   1
#> 236 IPD_Study Drug_A        0         1   1
#> 237 IPD_Study Drug_A        0         1   0
#> 238 IPD_Study Drug_A        1         0   0
#> 239 IPD_Study Drug_A        1         1   0
#> 240 IPD_Study Drug_A        0         1   1
#> 241 IPD_Study Drug_A        0         1   0
#> 242 IPD_Study Drug_A        1         0   0
#> 243 IPD_Study Drug_A        0         0   1
#> 244 IPD_Study Drug_A        0         0   1
#> 245 IPD_Study Drug_A        1         1   1
#> 246 IPD_Study Drug_A        1         0   1
#> 247 IPD_Study Drug_A        0         0   0
#> 248 IPD_Study Drug_A        1         1   1
#> 249 IPD_Study Drug_A        1         1   0
#> 250 IPD_Study Drug_A        1         1   1
#> 251 IPD_Study Drug_A        0         0   0
#> 252 IPD_Study Drug_A        1         0   1
#> 253 IPD_Study Drug_A        1         1   1
#> 254 IPD_Study Drug_A        0         0   0
#> 255 IPD_Study Drug_A        1         0   0
#> 256 IPD_Study Drug_A        1         0   1
#> 257 IPD_Study Drug_A        0         0   0
#> 258 IPD_Study Drug_A        0         0   1
#> 259 IPD_Study Drug_A        0         1   1
#> 260 IPD_Study Drug_A        0         0   0
#> 261 IPD_Study Drug_A        1         0   1
#> 262 IPD_Study Drug_A        1         0   0
#> 263 IPD_Study Drug_A        0         1   1
#> 264 IPD_Study Drug_A        1         1   1
#> 265 IPD_Study Drug_A        0         0   0
#> 266 IPD_Study Drug_A        1         0   0
#> 267 IPD_Study Drug_A        1         1   0
#> 268 IPD_Study Drug_A        1         1   0
#> 269 IPD_Study Drug_A        0         0   1
#> 270 IPD_Study Drug_A        0         0   1
#> 271 IPD_Study Drug_A        1         0   1
#> 272 IPD_Study Drug_A        1         1   0
#> 273 IPD_Study Drug_A        1         1   1
#> 274 IPD_Study Drug_A        1         0   1
#> 275 IPD_Study Drug_A        0         0   0
#> 276 IPD_Study Drug_A        1         0   0
#> 277 IPD_Study Drug_A        0         0   1
#> 278 IPD_Study Drug_A        0         1   1
#> 279 IPD_Study Drug_A        0         1   1
#> 280 IPD_Study Drug_A        1         1   1
#> 281 IPD_Study Drug_A        0         0   0
#> 282 IPD_Study Drug_A        0         1   0
#> 283 IPD_Study Drug_A        1         1   1
#> 284 IPD_Study Drug_A        0         0   1
#> 285 IPD_Study Drug_A        0         0   0
#> 286 IPD_Study Drug_A        1         1   0
#> 287 IPD_Study Drug_A        1         1   1
#> 288 IPD_Study Drug_A        1         0   0
#> 289 IPD_Study Drug_A        0         0   1
#> 290 IPD_Study Drug_A        0         0   0
#> 291 IPD_Study Drug_A        1         0   1
#> 292 IPD_Study Drug_A        0         0   0
#> 293 IPD_Study Drug_A        0         1   1
#> 294 IPD_Study Drug_A        0         0   1
#> 295 IPD_Study Drug_A        0         0   1
#> 296 IPD_Study Drug_A        0         1   0
#> 297 IPD_Study Drug_A        1         1   1
#> 298 IPD_Study Drug_A        1         0   1
#> 299 IPD_Study Drug_A        1         0   1
#> 300 IPD_Study Drug_A        1         0   1
#> 301 IPD_Study Drug_A        1         1   0
#> 302 IPD_Study Drug_A        1         0   1
#> 303 IPD_Study Drug_A        0         1   0
#> 304 IPD_Study Drug_A        1         1   0
#> 305 IPD_Study Drug_A        0         0   1
#> 306 IPD_Study Drug_A        1         1   0
#> 307 IPD_Study Drug_A        0         0   1
#> 308 IPD_Study Drug_A        1         1   1
#> 309 IPD_Study Drug_A        0         0   0
#> 310 IPD_Study Drug_A        0         1   0
#> 311 IPD_Study Drug_A        0         0   0
#> 312 IPD_Study Drug_A        0         1   0
#> 313 IPD_Study Drug_A        1         1   1
#> 314 IPD_Study Drug_A        0         1   1
#> 315 IPD_Study Drug_A        1         1   0
#> 316 IPD_Study Drug_A        0         1   1
#> 317 IPD_Study Drug_A        1         0   0
#> 318 IPD_Study Drug_A        1         0   0
#> 319 IPD_Study Drug_A        0         0   1
#> 320 IPD_Study Drug_A        0         1   0
#> 321 IPD_Study Drug_A        0         1   1
#> 322 IPD_Study Drug_A        1         0   0
#> 323 IPD_Study Drug_A        1         0   1
#> 324 IPD_Study Drug_A        0         0   1
#> 325 IPD_Study Drug_A        1         1   1
#> 326 IPD_Study Drug_A        0         1   0
#> 327 IPD_Study Drug_A        0         1   1
#> 328 IPD_Study Drug_A        0         0   1
#> 329 IPD_Study Drug_A        1         0   1
#> 330 IPD_Study Drug_A        0         0   0
#> 331 IPD_Study Drug_A        1         0   0
#> 332 IPD_Study Drug_A        0         0   1
#> 333 IPD_Study Drug_A        0         1   0
#> 334 IPD_Study Drug_A        0         1   1
#> 335 IPD_Study Drug_A        1         1   1
#> 336 IPD_Study Drug_A        0         1   0
#> 337 IPD_Study Drug_A        1         1   1
#> 338 IPD_Study Drug_A        1         1   0
#> 339 IPD_Study Drug_A        1         0   1
#> 340 IPD_Study Drug_A        0         0   0
#> 341 IPD_Study Drug_A        1         1   1
#> 342 IPD_Study Drug_A        1         1   1
#> 343 IPD_Study Drug_A        1         0   1
#> 344 IPD_Study Drug_A        1         1   1
#> 345 IPD_Study Drug_A        0         1   1
#> 346 IPD_Study Drug_A        1         0   0
#> 347 IPD_Study Drug_A        1         0   1
#> 348 IPD_Study Drug_A        1         1   1
#> 349 IPD_Study Drug_A        1         0   1
#> 350 IPD_Study Drug_A        1         0   1
#> 351 IPD_Study Drug_A        1         1   0
#> 352 IPD_Study Drug_A        0         0   1
#> 353 IPD_Study Drug_A        1         0   1
#> 354 IPD_Study Drug_A        1         0   0
#> 355 IPD_Study Drug_A        0         0   0
#> 356 IPD_Study Drug_A        0         1   1
#> 357 IPD_Study Drug_A        0         1   1
#> 358 IPD_Study Drug_A        1         1   1
#> 359 IPD_Study Drug_A        1         0   0
#> 360 IPD_Study Drug_A        0         0   1
#> 361 IPD_Study Drug_A        1         0   0
#> 362 IPD_Study Drug_A        1         0   1
#> 363 IPD_Study Drug_A        0         1   1
#> 364 IPD_Study Drug_A        1         0   0
#> 365 IPD_Study Drug_A        0         1   1
#> 366 IPD_Study Drug_A        1         0   1
#> 367 IPD_Study Drug_A        0         0   1
#> 368 IPD_Study Drug_A        0         0   1
#> 369 IPD_Study Drug_A        1         1   1
#> 370 IPD_Study Drug_A        0         1   0
#> 371 IPD_Study Drug_A        0         0   0
#> 372 IPD_Study Drug_A        1         1   0
#> 373 IPD_Study Drug_A        0         0   1
#> 374 IPD_Study Drug_A        1         0   1
#> 375 IPD_Study Drug_A        0         0   1
#> 376 IPD_Study Drug_A        1         0   1
#> 377 IPD_Study Drug_A        0         0   1
#> 378 IPD_Study Drug_A        1         0   0
#> 379 IPD_Study Drug_A        1         1   0
#> 380 IPD_Study Drug_A        1         0   1
#> 381 IPD_Study Drug_A        0         1   0
#> 382 IPD_Study Drug_A        0         1   1
#> 383 IPD_Study Drug_A        1         1   1
#> 384 IPD_Study Drug_A        1         0   0
#> 385 IPD_Study Drug_A        0         0   1
#> 386 IPD_Study Drug_A        0         0   1
#> 387 IPD_Study Drug_A        0         0   1
#> 388 IPD_Study Drug_A        1         0   0
#> 389 IPD_Study Drug_A        1         1   0
#> 390 IPD_Study Drug_A        1         0   0
#> 391 IPD_Study Drug_A        0         0   1
#> 392 IPD_Study Drug_A        0         0   0
#> 393 IPD_Study Drug_A        1         1   0
#> 394 IPD_Study Drug_A        1         0   1
#> 395 IPD_Study Drug_A        1         0   0
#> 396 IPD_Study Drug_A        0         1   0
#> 397 IPD_Study Drug_A        1         1   0
#> 398 IPD_Study Drug_A        0         0   0
#> 399 IPD_Study Drug_A        1         0   1
#> 400 IPD_Study Drug_A        1         0   1
#> 401 IPD_Study Drug_A        1         1   0
#> 402 IPD_Study Drug_A        1         1   0
#> 403 IPD_Study Drug_A        0         1   1
#> 404 IPD_Study Drug_A        1         0   1
#> 405 IPD_Study Drug_A        1         1   0
#> 406 IPD_Study Drug_A        1         0   1
#> 407 IPD_Study Drug_A        1         0   0
#> 408 IPD_Study Drug_A        1         1   1
#> 409 IPD_Study Drug_A        1         1   0
#> 410 IPD_Study Drug_A        0         0   1
#> 411 IPD_Study Drug_A        0         1   1
#> 412 IPD_Study Drug_A        1         0   0
#> 413 IPD_Study Drug_A        1         1   0
#> 414 IPD_Study Drug_A        0         0   0
#> 415 IPD_Study Drug_A        1         1   0
#> 416 IPD_Study Drug_A        1         1   0
#> 417 IPD_Study Drug_A        1         1   1
#> 418 IPD_Study Drug_A        1         0   0
#> 419 IPD_Study Drug_A        0         1   1
#> 420 IPD_Study Drug_A        1         0   1
#> 421 IPD_Study Drug_A        1         1   0
#> 422 IPD_Study Drug_A        1         1   1
#> 423 IPD_Study Drug_A        0         1   0
#> 424 IPD_Study Drug_A        1         1   1
#> 425 IPD_Study Drug_A        1         1   0
#> 426 IPD_Study Drug_A        1         0   1
#> 427 IPD_Study Drug_A        1         1   0
#> 428 IPD_Study Drug_A        1         1   0
#> 429 IPD_Study Drug_A        1         1   1
#> 430 IPD_Study Drug_A        0         0   0
#> 431 IPD_Study Drug_A        0         1   1
#> 432 IPD_Study Drug_A        1         0   0
#> 433 IPD_Study Drug_A        0         0   1
#> 434 IPD_Study Drug_A        1         0   0
#> 435 IPD_Study Drug_A        0         0   0
#> 436 IPD_Study Drug_A        1         1   0
#> 437 IPD_Study Drug_A        0         1   1
#> 438 IPD_Study Drug_A        1         1   1
#> 439 IPD_Study Drug_A        0         1   0
#> 440 IPD_Study Drug_A        1         0   0
#> 441 IPD_Study Drug_A        1         0   0
#> 442 IPD_Study Drug_A        1         1   0
#> 443 IPD_Study Drug_A        1         0   1
#> 444 IPD_Study Drug_A        1         1   1
#> 445 IPD_Study Drug_A        1         0   0
#> 446 IPD_Study Drug_A        0         1   0
#> 447 IPD_Study Drug_A        1         0   0
#> 448 IPD_Study Drug_A        1         0   1
#> 449 IPD_Study Drug_A        1         0   0
#> 450 IPD_Study Drug_A        1         0   1
#> 451 IPD_Study Drug_A        0         1   1
#> 452 IPD_Study Drug_A        1         0   1
#> 453 IPD_Study Drug_A        1         0   1
#> 454 IPD_Study Drug_A        0         0   0
#> 455 IPD_Study Drug_A        1         1   1
#> 456 IPD_Study Drug_A        1         0   1
#> 457 IPD_Study Drug_A        0         0   0
#> 458 IPD_Study Drug_A        0         1   0
#> 459 IPD_Study Drug_A        0         0   1
#> 460 IPD_Study Drug_A        0         0   0
#> 461 IPD_Study Drug_A        1         1   1
#> 462 IPD_Study Drug_A        0         1   1
#> 463 IPD_Study Drug_A        0         1   0
#> 464 IPD_Study Drug_A        1         0   1
#> 465 IPD_Study Drug_A        0         0   0
#> 466 IPD_Study Drug_A        1         1   1
#> 467 IPD_Study Drug_A        0         0   0
#> 468 IPD_Study Drug_A        1         0   1
#> 469 IPD_Study Drug_A        1         1   0
#> 470 IPD_Study Drug_A        1         0   0
#> 471 IPD_Study Drug_A        0         1   1
#> 472 IPD_Study Drug_A        1         1   1
#> 473 IPD_Study Drug_A        1         1   1
#> 474 IPD_Study Drug_A        1         1   1
#> 475 IPD_Study Drug_A        0         1   0
#> 476 IPD_Study Drug_A        0         0   1
#> 477 IPD_Study Drug_A        0         0   1
#> 478 IPD_Study Drug_A        1         1   1
#> 479 IPD_Study Drug_A        0         0   0
#> 480 IPD_Study Drug_A        0         0   1
#> 481 IPD_Study Drug_A        0         1   0
#> 482 IPD_Study Drug_A        1         0   1
#> 483 IPD_Study Drug_A        1         1   0
#> 484 IPD_Study Drug_A        1         0   0
#> 485 IPD_Study Drug_A        0         0   1
#> 486 IPD_Study Drug_A        0         0   0
#> 487 IPD_Study Drug_A        0         1   1
#> 488 IPD_Study Drug_A        1         1   0
#> 489 IPD_Study Drug_A        1         0   0
#> 490 IPD_Study Drug_A        0         0   1
#> 491 IPD_Study Drug_A        1         0   1
#> 492 IPD_Study Drug_A        0         0   1
#> 493 IPD_Study Drug_A        1         1   1
#> 494 IPD_Study Drug_A        0         0   1
#> 495 IPD_Study Drug_A        1         0   1
#> 496 IPD_Study Drug_A        1         0   0
#> 497 IPD_Study Drug_A        0         0   1
#> 498 IPD_Study Drug_A        0         1   0
#> 499 IPD_Study Drug_A        1         0   0
#> 500 IPD_Study Drug_A        0         0   0
#> 
#> $n
#> [1] 500
#> 
#> $treatment
#> [1] "Drug_A"
#> 
#> $covariates
#> [1] "age_group" "sex"      
#> 
#> $family
#> [1] "binomial"
#> 
#> $type
#> [1] "ipd"
#> 
#> $n_events
#> [1] 303
#> 
#> attr(,"class")
#> [1] "mlumr_ipd" "list"
```

### Continuous outcomes

For continuous outcomes, use `family = "normal"`. The IPD frame needs a
numeric outcome column:

``` r

trial_a_normal <- data.frame(
  patient_id = 1:500,
  treatment = "Drug_A",
  score = rnorm(500, mean = 3.0, sd = 1.2),
  age_group = rbinom(500, 1, 0.4),
  sex = rbinom(500, 1, 0.55)
)

ipd_normal <- set_ipd(
  data = trial_a_normal,
  treatment = "treatment",
  outcome = "score",
  covariates = c("age_group", "sex"),
  family = "normal"
)
```

### Count outcomes

For Poisson count data, use `family = "poisson"` and supply an
`exposure` column:

``` r

trial_a_poisson <- data.frame(
  patient_id = 1:500,
  treatment = "Drug_A",
  events = rpois(500, lambda = 0.8),
  person_years = runif(500, 0.5, 2.0),
  age_group = rbinom(500, 1, 0.4),
  sex = rbinom(500, 1, 0.55)
)

ipd_poisson <- set_ipd(
  data = trial_a_poisson,
  treatment = "treatment",
  outcome = "events",
  covariates = c("age_group", "sex"),
  family = "poisson",
  exposure = "person_years"
)
```

### Validation

[`set_ipd()`](https://choxos.github.io/mlumr/reference/set_ipd.md)
checks that:

- All specified columns exist in the data
- The outcome matches the family (binary for binomial, numeric for
  normal, non-negative integers for Poisson)
- Missing values are handled (rows dropped with a warning)

``` r

# This will error: non-binary outcome with binomial family
bad_data <- data.frame(trt = "A", outcome = c(0, 1, 2), x = rnorm(3))
set_ipd(bad_data, "trt", "outcome", "x")
#> Error:
#> ! `outcome` must be binary (0/1) for binomial family
```

## Setting up AgD

AgD should be a data frame with one row per study/subgroup, containing:

- Treatment indicator
- Outcome summaries (depend on `family`):
  - **Binomial**: sample size (`outcome_n`) and number of events
    (`outcome_r`)
  - **Normal**: mean outcome (`outcome_mean`) and standard error
    (`outcome_se`), optionally sample size (`outcome_n`)
  - **Poisson**: number of events (`outcome_r`) and total exposure
    (`outcome_E`)
- Covariate means/proportions

``` r

# Example: Trial B data (comparator treatment)
trial_b <- data.frame(
  study = "Trial_B",
  treatment = "Drug_B",
  n_total = 400,
  n_events = 160,
  age_group_mean = 0.35,   # proportion in "old" group
  sex_prop = 0.50           # proportion female
)

agd <- set_agd(
  data = trial_b,
  treatment = "treatment",
  outcome_n = "n_total",
  outcome_r = "n_events",
  cov_means = c("age_group_mean", "sex_prop"),
  cov_types = c("binary", "binary")
)
```

### AgD for continuous outcomes

``` r

agd_normal <- set_agd(
  data = data.frame(
    trt = "Drug_B", y_mean = 3.2, se = 0.15, n = 400,
    age_group_mean = 0.35, sex_prop = 0.50
  ),
  treatment = "trt",
  family = "normal",
  outcome_mean = "y_mean",
  outcome_se = "se",
  outcome_n = "n",
  cov_means = c("age_group_mean", "sex_prop"),
  cov_types = c("binary", "binary")
)
```

**Scale requirement for normal outcomes.**
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
assumes the AgD mean (`outcome_mean`) and SE (`outcome_se`) are on the
**arithmetic (original, untransformed) scale**, regardless of whether
[`mlumr()`](https://choxos.github.io/mlumr/reference/mlumr.md) is later
called with `link = "identity"` or `link = "log"`. If a publication
reports only a log-scale mean and SD, or a geometric mean,
back-transform before calling
[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md) and
propagate uncertainty via the delta method; passing log-scale summaries
silently biases the posterior because the Stan likelihood is
`normal(E[exp(eta)], se_agd)` under the log link, expecting the AgD
quantities already on the outcome scale.

### AgD for count outcomes

``` r

agd_poisson <- set_agd(
  data = data.frame(
    trt = "Drug_B", n_events = 120, person_years = 800,
    age_group_mean = 0.35, sex_prop = 0.50
  ),
  treatment = "trt",
  family = "poisson",
  outcome_r = "n_events",
  outcome_E = "person_years",
  cov_means = c("age_group_mean", "sex_prop"),
  cov_types = c("binary", "binary")
)
```

### Covariate naming

[`set_agd()`](https://choxos.github.io/mlumr/reference/set_agd.md)
automatically strips `_mean` and `_prop` suffixes from covariate names
to match the IPD covariate names:

- `"age_group_mean"` -\> covariate name `"age_group"`
- `"sex_prop"` -\> covariate name `"sex"`

Make sure the resulting names match those in your IPD.

### Continuous covariates

For continuous covariates, also supply standard deviations:

``` r

agd_continuous <- set_agd(
  data = data.frame(
    trt = "B", n_total = 400, n_events = 160,
    bmi_mean = 25.3, bmi_sd = 4.2
  ),
  treatment = "trt",
  outcome_n = "n_total",
  outcome_r = "n_events",
  cov_means = "bmi_mean",
  cov_sds = "bmi_sd",
  cov_types = "continuous"
)
```

## Combining data

``` r

dat <- combine_data(ipd, agd)
print(dat)
#> Unanchored Comparison Data (Binary)
#> ====================================
#> 
#> Index treatment (IPD): Drug_A 
#>   N = 500 
#>   Events = 303 (60.6%) 
#> 
#> Comparator treatment (AgD): Drug_B 
#>   N = 400 
#>   Events = 160 (40.0%) 
#> 
#> Covariates ( 2 ): age_group, sex 
#> Integration points: not yet added (use add_integration())
```

## Adding integration points

Integration points are needed for ML-UMR’s numerical integration over
the comparator population’s covariate distribution. They are generated
using Sobol quasi-Monte Carlo sequences with a Gaussian copula to
respect covariate correlations.

### Basic usage

``` r

dat <- add_integration(
  dat,
  n_int = 64,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)
```

Key points:

- **`n_int`**: Number of integration points (powers of 2 recommended:
  32, 64, 128, 256). More points = better accuracy but slower Stan
  fitting.
- **[`distr()`](https://choxos.github.io/mlumr/reference/distr.md)**:
  Wraps a quantile function with parameters. Parameters can reference
  columns in the AgD data (e.g., `age_group_mean`).
- **`qbern`**: Bernoulli quantile function (provided by mlumr) for
  binary covariates. Use `qnorm` for continuous covariates.

### Distribution types

``` r

# Binary covariate
age = distr(qbern, prob = age_mean)

# Continuous covariate (normal)
bmi = distr(qnorm, mean = bmi_mean, sd = bmi_sd)

# Continuous covariate (gamma)
biomarker = distr(qgamma, shape = 2, rate = 0.5)
```

### Correlation handling

By default, correlations between covariates are estimated from the IPD
using Spearman rank correlation, then adjusted for the Gaussian copula.
For binary and mixed discrete/continuous covariate pairs, this
adjustment is a pragmatic approximation — exact calibration of the
latent Gaussian correlation depends on marginal prevalences, which are
not used in the current transform. For most practical datasets this
provides adequate integration-point distributions, but users with strong
prior information on covariate correlations can supply their own matrix
via the `cor` argument.

``` r

# Default: Spearman correlation from IPD
dat <- add_integration(dat, n_int = 64,
                       age_group = distr(qbern, prob = age_group_mean),
                       sex = distr(qbern, prob = sex_mean))

# Supply your own correlation matrix
my_cor <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
dat <- add_integration(dat, n_int = 64, cor = my_cor,
                       age_group = distr(qbern, prob = age_group_mean),
                       sex = distr(qbern, prob = sex_mean))

# No correlation adjustment
dat <- add_integration(dat, n_int = 64, cor_adjust = "none",
                       age_group = distr(qbern, prob = age_group_mean),
                       sex = distr(qbern, prob = sex_mean))
```

### Inspecting integration points

``` r

# Expand to long format
int_df <- unnest_integration(dat)
head(int_df)
#>   age_group sex .int_id .agd_row
#> 1         0   0       1        1
#> 2         1   0       2        1
#> 3         0   1       3        1
#> 4         0   0       4        1
#> 5         1   1       5        1
#> 6         0   0       6        1
```

Use
[`check_integration()`](https://choxos.github.io/mlumr/reference/check_integration.md)
to compare the generated integration points against the requested AgD
marginal distributions and correlation structure:

``` r

check_integration(
  dat,
  age_group = distr(qbern, prob = age_group_mean),
  sex = distr(qbern, prob = sex_mean)
)
#> Integration check: n_int = 64 vs 128
#> Marginals -- max relative difference: 0.0159
#> CAUTION: 1-5%% marginal relative difference. Consider increasing n_int.
#> Joint -- max |cor(current) - cor(doubled)|: 0.0113
#> OK joint: pairwise correlations agree within 0.05.
```

This is especially important when there are several covariates, strong
correlations, binary covariates with prevalences near 0 or 1, or a small
number of integration points.

## Notes on STC and naive

The STC and naive methods do **not** require integration points. The STC
uses integration points if available (for better marginalization), but
falls back to AgD covariate means if they are not. You can call
[`stc()`](https://choxos.github.io/mlumr/reference/stc.md) and
[`naive()`](https://choxos.github.io/mlumr/reference/naive.md)
immediately after
[`combine_data()`](https://choxos.github.io/mlumr/reference/combine_data.md).

``` r

# These work without add_integration()
dat_no_int <- combine_data(ipd, agd)
naive_result <- naive(dat_no_int)
stc_result <- stc(dat_no_int)
```
