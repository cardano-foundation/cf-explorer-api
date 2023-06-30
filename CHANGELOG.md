# Changelog

## [0.3.14](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.13...v0.3.14) (2023-06-29)


### Features

* add code of conduct and contributing information (MET-1380) ([2183e6d](https://github.com/cardano-foundation/cf-explorer-api/commit/2183e6ddfe094d56921dd0f07795319c1523bfb2))


### Bug Fixes

* **gha:** fixed PR builds ([575c85b](https://github.com/cardano-foundation/cf-explorer-api/commit/575c85b6d292815e119d74430932b0c6c0c6358d))
* remove default value ([91ae401](https://github.com/cardano-foundation/cf-explorer-api/commit/91ae40129390ce93e7c3040e48c528b268563f24))

## [0.3.13](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.12...v0.3.13) (2023-06-26)


### Bug Fixes

* update README.md trigger release ([79afdeb](https://github.com/cardano-foundation/cf-explorer-api/commit/79afdebb9619c66372377fc579d93b10c93fdd1e))

## [0.3.12](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.11...v0.3.12) (2023-06-22)


### Bug Fixes

* **gha:** fix condition for main branch workflow trigger ([3f271c4](https://github.com/cardano-foundation/cf-explorer-api/commit/3f271c41a0f3721a641baf3e1b5f2615531da28c))
* **gha:** fix condition for main branch workflow trigger ([4570edf](https://github.com/cardano-foundation/cf-explorer-api/commit/4570edf402fc3f847a28308561933b070d3f1019))
* **gha:** fix condition for main branch workflow trigger ([57c9ce2](https://github.com/cardano-foundation/cf-explorer-api/commit/57c9ce2fccfa1e353e348935c290ebdc2c852d18))
* remove default sort in pool-reward query ([1d6f850](https://github.com/cardano-foundation/cf-explorer-api/commit/1d6f850d1a0a4a2517819488a10341566add2e6a))

## [0.3.11](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.10...v0.3.11) (2023-06-20)


### Features

* [MET-1139] add logic optimize chart token ([b9a8ad3](https://github.com/cardano-foundation/cf-explorer-api/commit/b9a8ad39be257a24ad1c8b38f51f146db5057613))
* [MET-1139] update logic query data token balance to handler case data interrupted when consumer stopped ([2e2cf08](https://github.com/cardano-foundation/cf-explorer-api/commit/2e2cf08202f9c8b98e7ff3fa391dd4e97d472fce))
* [MET-1143] handle abnormal case for agg_address_tx_balance ([82e0da3](https://github.com/cardano-foundation/cf-explorer-api/commit/82e0da352b91b473870a7e6e775b098cc3950476))
* [MET-1143] Optimize chart stake and balance ([1b95723](https://github.com/cardano-foundation/cf-explorer-api/commit/1b95723ceacd4a5307a642fe466d322a5d7b44e9))
* [MET-1143] remove redundant condition ([8a833bc](https://github.com/cardano-foundation/cf-explorer-api/commit/8a833bc921ead41b9a1331ff58a05d2ceb76a132))
* [MET-1143] update code ([0f96e13](https://github.com/cardano-foundation/cf-explorer-api/commit/0f96e13e37f3759e73f16f3fb0eeac18be8c6a9f))
* [MET-1154] add thread pool config ([5e1ad11](https://github.com/cardano-foundation/cf-explorer-api/commit/5e1ad11aa5e819a5a1ea4e053a3d6e4a69870e48))
* [MET-1154] optimize call token filter api ([4ef2fde](https://github.com/cardano-foundation/cf-explorer-api/commit/4ef2fde3c376018bee8d62629ae0b228b803a9fa))
* [MET-1199] Optimize chart for stake/address balance ([0c08f91](https://github.com/cardano-foundation/cf-explorer-api/commit/0c08f91cc1a57e6ab4dfa5074395b31b01399c04))
* [MET-1348] Move cron job top delegators to Schedule ([3a655b3](https://github.com/cardano-foundation/cf-explorer-api/commit/3a655b35e4fc820f47fd593c04c992648fd27fc6))
* [MET-665] optimize transaction stake ([25d43ef](https://github.com/cardano-foundation/cf-explorer-api/commit/25d43effcee27b16ddf49675b648f5b60c9a4129))
* add new logic to check reward data of stake key ([9987389](https://github.com/cardano-foundation/cf-explorer-api/commit/99873895b9e244fd017f81b317328fa9f1bfed3c))
* make test coverage result public available ([c069dae](https://github.com/cardano-foundation/cf-explorer-api/commit/c069dae24408a589d7859f0161d550c93be11947))
* met-239 current epoch time ([00ad9e5](https://github.com/cardano-foundation/cf-explorer-api/commit/00ad9e59169c78ceb0adb5a0072b923aa29ec387))
* met-697 add filter ([48538ab](https://github.com/cardano-foundation/cf-explorer-api/commit/48538ab40ff7df9f3f25b2656cba608a2c159159))
* met-697 add handle genesis when filter ([c1a5e24](https://github.com/cardano-foundation/cf-explorer-api/commit/c1a5e24db954ebeb9e634ca229584eb96f02de57))
* met-697 change condition filter ([d251fb8](https://github.com/cardano-foundation/cf-explorer-api/commit/d251fb8dfb270fe96c3ec2525b07ca343a1990e1))
* met-697 optimize filter by epoch process ([f63b8a6](https://github.com/cardano-foundation/cf-explorer-api/commit/f63b8a63cfad1f218b6f78a914de0fca20086e77))
* MET-753 script verification function ([f003dda](https://github.com/cardano-foundation/cf-explorer-api/commit/f003dda3bb24072576b1b525f04ff301a3678002))
* met-777 moving logic of export file to cardano-schedule ([9b47efb](https://github.com/cardano-foundation/cf-explorer-api/commit/9b47efbd18ba4e93f90bca99f90554c0b7ad236e))
* update apis for pool lifecycle ([aa811a2](https://github.com/cardano-foundation/cf-explorer-api/commit/aa811a26687382e85d0bc99e00adb154a27cca35))


### Bug Fixes

* add check active lifecycle for stake key ([725852d](https://github.com/cardano-foundation/cf-explorer-api/commit/725852d655e526693b199963d4bd268a8d1a7985))
* add code for pool-registration, pool-de-registration apis ([a6d59a5](https://github.com/cardano-foundation/cf-explorer-api/commit/a6d59a5f33973c5c21f67d439181dfc1c29f4434))
* add hold for pool lifecycle registration ([f06622c](https://github.com/cardano-foundation/cf-explorer-api/commit/f06622c713f3119d62a341ef21412307190af7eb))
* add logic fetch ada_pots from koios ([9aa3e99](https://github.com/cardano-foundation/cf-explorer-api/commit/9aa3e99dc1a47bcbdcda321aef806fc5ad32f937))
* add logic koios for pool apis relate ([00166c0](https://github.com/cardano-foundation/cf-explorer-api/commit/00166c0141998074d1810ab89a32e0bd3201fa9a))
* add pool-history fetching flow to pool-size pool-report ([20fea0c](https://github.com/cardano-foundation/cf-explorer-api/commit/20fea0c4d1ccb186683fca67c20050555e009316))
* change logic stake registration, stake deregistration and pool delegation of stake key ([241a395](https://github.com/cardano-foundation/cf-explorer-api/commit/241a3951d9c1e15e5a471708fd120fdc32d6cab8))
* change logic to fix incorrect total output and total fee of block ([474924c](https://github.com/cardano-foundation/cf-explorer-api/commit/474924cb1beee371dc2a1b4af9e3b40471e3257e))
* change variable in SQL to different with others ([5eed8f1](https://github.com/cardano-foundation/cf-explorer-api/commit/5eed8f104921fbd19d17994434ea533432f4bcb1))
* fix after review PR ([55b0830](https://github.com/cardano-foundation/cf-explorer-api/commit/55b08307f8e1f5db0443a5d11f1279d103a4abbc))
* fix bugs relate koios flow ([4b4bb63](https://github.com/cardano-foundation/cf-explorer-api/commit/4b4bb6370dc3e1bc0198f7b724003b89455b10a0))
* fix holder of token with quantity greater than 0 ([fb770fc](https://github.com/cardano-foundation/cf-explorer-api/commit/fb770fc87182235ef222c683193d80f3b39ffe66))
* MET-1148 add instantaneous rewards to tx detail api ([8054678](https://github.com/cardano-foundation/cf-explorer-api/commit/8054678f80d263a522611314b1e53b0e5c8ac738))
* MET-1238 add delegations list api ([057b4fc](https://github.com/cardano-foundation/cf-explorer-api/commit/057b4fc17921c78f9d99c6d961e66510a0ef9067))
* MET-1238 add instantaneous rewards list api ([aac3bbe](https://github.com/cardano-foundation/cf-explorer-api/commit/aac3bbe4f9fd2c0f6cf165f388244536a296f0e0))
* met-697 missing data when filter by time in preprod ([49aad83](https://github.com/cardano-foundation/cf-explorer-api/commit/49aad8307d822c1c2ff39dcaea95eca1b55bcd4c))
* rename interface ([5a66c39](https://github.com/cardano-foundation/cf-explorer-api/commit/5a66c39b74b89a5e96b7a0dcfdb7af0e23fb7367))

## [0.3.10](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.9...v0.3.10) (2023-05-31)


### Features

* met-239 tx chart widget ([a035d54](https://github.com/cardano-foundation/cf-explorer-api/commit/a035d54682468d68212ae71a0690b10e9d7fe7c0))
* met-697 get histories filter by protocol type ([ecc5934](https://github.com/cardano-foundation/cf-explorer-api/commit/ecc59347c5d31595932c46d86b5c67e916972f4e))
* met-697 old protocol param ([24d05b7](https://github.com/cardano-foundation/cf-explorer-api/commit/24d05b745d91a0c0f4f64cd86deb90042dfc1fbe))
* met-697 protocol parameters ([64388a3](https://github.com/cardano-foundation/cf-explorer-api/commit/64388a312cdc015843860a63d76ca5ecb81d7a47))
* met-741 pool id report function ([f754951](https://github.com/cardano-foundation/cf-explorer-api/commit/f754951037bfbb87227d6bc1ef9713b9961735f9))
* stake key report function ([627ef8c](https://github.com/cardano-foundation/cf-explorer-api/commit/627ef8c11f37e716a15ff5945c8cefce315df189))


### Bug Fixes

* fix conflict ([12076bd](https://github.com/cardano-foundation/cf-explorer-api/commit/12076bd1b330674db4dfb1372a1b68b68de7e4a0))
* fix unit test after edit code ([9df28bb](https://github.com/cardano-foundation/cf-explorer-api/commit/9df28bb4f1176cc44ea33eeb0b4d260c3118bf4e))
* MET-400 optimize query top ([1518892](https://github.com/cardano-foundation/cf-explorer-api/commit/1518892418600fae5318e9008dc48fd440ae9a6c))
* not response epochNo ([9f9f3a4](https://github.com/cardano-foundation/cf-explorer-api/commit/9f9f3a4b4279389520710ecb443210a36ee94e9a))
* not to choose lastest ([ef1e6c1](https://github.com/cardano-foundation/cf-explorer-api/commit/ef1e6c187a94f3f901b43073c56858035322de05))
* show registration and pool update in SPO incorrect ([c03c834](https://github.com/cardano-foundation/cf-explorer-api/commit/c03c8344b0f69399e8d57108c35722cd73fa6340))
* transaction hash filter not working ([db58fd1](https://github.com/cardano-foundation/cf-explorer-api/commit/db58fd16b93e12edb07bc41ce3a23487ed4b9735))

## [0.3.9](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.8...v0.3.9) (2023-05-16)


### Features

* setup authentication for report function ([be325cd](https://github.com/cardano-foundation/cf-explorer-api/commit/be325cdb64f0646ba19b3fe85dd37b3606e94b87))


### Bug Fixes

* bug top delegations error 500 ([b2e92bf](https://github.com/cardano-foundation/cf-explorer-api/commit/b2e92bf3140d53d7725753d66f33336fc3701d25))
* ignore authentication check with unit test ([af0f10a](https://github.com/cardano-foundation/cf-explorer-api/commit/af0f10a3af3738c58e357960c1ada971634ef2aa))

## [0.3.8](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.7...v0.3.8) (2023-05-15)


### Bug Fixes

* bump common versions ([3701cbb](https://github.com/cardano-foundation/cf-explorer-api/commit/3701cbb93fabb0a1d55dac63be8edf1188a55fb9))

## [0.3.7](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.7-SNAPSHOT...v0.3.7) (2023-05-15)


### Features

* added Standalone and Cluster mode for Redis ([3d82da8](https://github.com/cardano-foundation/cf-explorer-api/commit/3d82da83918ca03f02306b2bb4c6f426064358fc))
* MET-665 revise transaction data shown on address details and transaction details pages ([2cdc9d5](https://github.com/cardano-foundation/cf-explorer-api/commit/2cdc9d57ca171857f0977234cb6a08e4eec3c75e))


### Bug Fixes

* MET-1071 transaction detail pool certificates display pool owner wrong ([d778f37](https://github.com/cardano-foundation/cf-explorer-api/commit/d778f370b4848bc27c91681d483f7f180d62cf5f))

## [0.3.7-SNAPSHOT](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.5...v0.3.7-SNAPSHOT) (2023-05-05)


### Features

* add test and build pipeline ([5a128a9](https://github.com/cardano-foundation/cf-explorer-api/commit/5a128a9f7c9d3f526a8aa442dadaede529b25410))


### Bug Fixes

* force release run ([66a93fe](https://github.com/cardano-foundation/cf-explorer-api/commit/66a93fe19cacbd7c3624fcd5ef5ec9187472d23d))
* increase jdk version in Dockerfile ([6ff0798](https://github.com/cardano-foundation/cf-explorer-api/commit/6ff0798c77d241d62855775c737b50fd91fc8efa))
* MET-438 add total volume, number of holder and optimize query ([ed705dd](https://github.com/cardano-foundation/cf-explorer-api/commit/ed705dd5929b71504ca68c14cabcf34aecfbdd16))
* remove release-please ([9717ee2](https://github.com/cardano-foundation/cf-explorer-api/commit/9717ee2615a75c92922fe69c3aec59f8c7642a82))
* repair dockerfile ([6822068](https://github.com/cardano-foundation/cf-explorer-api/commit/682206812bfe96eb3bdcd6def1db96d6b92f60ba))


### Miscellaneous Chores

* release 0.3.7-SNAPSHOT ([cb08f37](https://github.com/cardano-foundation/cf-explorer-api/commit/cb08f371a3d45c4b539a31444beb509db3eb3943))

## [0.3.7-SNAPSHOT](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.5...v0.3.7-SNAPSHOT) (2023-05-05)


### Bug Fixes

* remove release-please ([9717ee2](https://github.com/cardano-foundation/cf-explorer-api/commit/9717ee2615a75c92922fe69c3aec59f8c7642a82))
* repair dockerfile ([6822068](https://github.com/cardano-foundation/cf-explorer-api/commit/682206812bfe96eb3bdcd6def1db96d6b92f60ba))


### Miscellaneous Chores

* release 0.3.7-SNAPSHOT ([cb08f37](https://github.com/cardano-foundation/cf-explorer-api/commit/cb08f371a3d45c4b539a31444beb509db3eb3943))

## [0.3.6-SNAPSHOT](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.5...v0.3.6-SNAPSHOT) (2023-05-05)


### Features

* add test and build pipeline ([5a128a9](https://github.com/cardano-foundation/cf-explorer-api/commit/5a128a9f7c9d3f526a8aa442dadaede529b25410))


### Bug Fixes

* MET-438 add total volume, number of holder and optimize query ([ed705dd](https://github.com/cardano-foundation/cf-explorer-api/commit/ed705dd5929b71504ca68c14cabcf34aecfbdd16))
