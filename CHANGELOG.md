# Changelog

## 1.6.0 (2024-07-29)


### Features

* add block producer to block details ([7f2fc3d](https://github.com/cardano-foundation/cf-explorer-api/commit/7f2fc3db3ac844a8fba3ceff29649d5900861581))
* add cc voting chart api ([520f1ad](https://github.com/cardano-foundation/cf-explorer-api/commit/520f1ad5ec888ce1ed0174d87ad13921fb00a334))
* add DRep filter api ([58e6cd1](https://github.com/cardano-foundation/cf-explorer-api/commit/58e6cd15546e922e80cad63c940a674bb50baa30))
* add DRep overview header ([44fffc8](https://github.com/cardano-foundation/cf-explorer-api/commit/44fffc8348109e807e1fd8ad45bc593d8a0f7639))
* add field for created report request ([67d7646](https://github.com/cardano-foundation/cf-explorer-api/commit/67d76468b00d17272dd8cad54ab01243c468bbde))
* add GovernanceTxIT test ([89d180b](https://github.com/cardano-foundation/cf-explorer-api/commit/89d180b4cc2760e02bf318a910002608fff3bc5d))
* add new ls agg dbsource configuration ([e8208d0](https://github.com/cardano-foundation/cf-explorer-api/commit/e8208d07a8806d17b5d43a99bc6e78cb3bb77dd0))
* add NO_TX_PURPOSE in tx_purpose filter ([5632ef4](https://github.com/cardano-foundation/cf-explorer-api/commit/5632ef4369c14db8558ab477addcd33fe04cc98f))
* add script type in token list response ([f973820](https://github.com/cardano-foundation/cf-explorer-api/commit/f97382021198571df3b8c81b5396a6178ed9786b))
* apply Script Controller with ledger sync v2 ([5a06962](https://github.com/cardano-foundation/cf-explorer-api/commit/5a06962b7bba2ae4060c3df08231688706ea3c81))
* caching MetadataBolnisi and add api get winery data by txHash and wineryId ([db023f3](https://github.com/cardano-foundation/cf-explorer-api/commit/db023f3d8cb3633d86808b28055aacce18649e80))
* caculated active stake of DRep and Pool by utxo balance ([ef13526](https://github.com/cardano-foundation/cf-explorer-api/commit/ef13526d9fda2782c4d0a2a1803657b7ba5bcce6))
* drep certificates history ([c18063b](https://github.com/cardano-foundation/cf-explorer-api/commit/c18063b5ebeef338e635ba740e86df0e529a530b))
* drep certificates history ([502e3f9](https://github.com/cardano-foundation/cf-explorer-api/commit/502e3f91a84481811069717588fe27585c5f00a9))
* filter by anchorText ([52885f8](https://github.com/cardano-foundation/cf-explorer-api/commit/52885f867f5f016b350070b336ba466b764ffdaf))
* filter for pool overview table ([a31af49](https://github.com/cardano-foundation/cf-explorer-api/commit/a31af494ed2780ece4dfd1c0594da3926f363ff6))
* filter type by votertype and find by poolView ([9e8273f](https://github.com/cardano-foundation/cf-explorer-api/commit/9e8273fa359ea676f0013666340d91a39dd8d84c))
* fix logic ([d68656c](https://github.com/cardano-foundation/cf-explorer-api/commit/d68656c9a4763d46413b9e41cd3de78aa4526b15))
* get CC threshold on gov-action in genesis file ([212f825](https://github.com/cardano-foundation/cf-explorer-api/commit/212f825fed39bc1c2c74633a8acda8da7add033a))
* get delegators who delegated to drep ([5f41396](https://github.com/cardano-foundation/cf-explorer-api/commit/5f413962a6c99d1132cc1bd68f0a3f57487916c9))
* get drep details ([dbd3cc7](https://github.com/cardano-foundation/cf-explorer-api/commit/dbd3cc73f67c2028f5cd1e9dc1d6a08ca41c0834))
* get gov actions that voted by drep or pool ([419c491](https://github.com/cardano-foundation/cf-explorer-api/commit/419c491435c1574c79c299f2bc10a5f5580e8dc1))
* get gov actions that voted by drep or pool ([019f198](https://github.com/cardano-foundation/cf-explorer-api/commit/019f198ff7f6c46a776099ffae88d360858e80bb))
* get vote chart of governance action ([4db74d2](https://github.com/cardano-foundation/cf-explorer-api/commit/4db74d2695e85f9e6d16bfcf765a548a50a43b0d))
* gov action details ([670697d](https://github.com/cardano-foundation/cf-explorer-api/commit/670697d9af322a2c244712ead83a45b813ab3cb7))
* handle value format for cip20 & cip83 ([e70659f](https://github.com/cardano-foundation/cf-explorer-api/commit/e70659f362f63a2bf57b4b053b1ebed9e5047a43))
* handle value format for cip25 ([1c8d2d2](https://github.com/cardano-foundation/cf-explorer-api/commit/1c8d2d28ee6d7f734f87c4f0127db0f78db30f4a))
* handle value format for cip60 ([214bf0d](https://github.com/cardano-foundation/cf-explorer-api/commit/214bf0d0d161886f9b0e8f492b0f05b54c72e187))
* include date-time pattern in create report request ([267d1e6](https://github.com/cardano-foundation/cf-explorer-api/commit/267d1e6b9477d89281d4bdf2a28f42e22525cc4e))
* listen "new_block" from pg_notify and publish to websocket ([dd78d37](https://github.com/cardano-foundation/cf-explorer-api/commit/dd78d374e968e4afa63b919616bd53d341ad80f6))
* MET-1503 update order of cip-83 metadata ([49b104e](https://github.com/cardano-foundation/cf-explorer-api/commit/49b104ec2340a409c05fa755359945e8fe1bd070))
* MET-1703-validate-bolnisi-metadata ([925ac6a](https://github.com/cardano-foundation/cf-explorer-api/commit/925ac6ad464bfa309786062770c92448694f3d10))
* MET-1761 moving logic verify native script to the explorer database ([0350d6d](https://github.com/cardano-foundation/cf-explorer-api/commit/0350d6d86538f1d51796616a4716b09d871000d7))
* MET-1858 update condition after and before is null to check native script when time-locked condition is open ([9d6f75f](https://github.com/cardano-foundation/cf-explorer-api/commit/9d6f75f4b62758490d6d6986e885ee4f214adc48))
* MET-1868 add default sort for native script tokens ([998e57f](https://github.com/cardano-foundation/cf-explorer-api/commit/998e57f37052ac84d41b9967f2984a9c60afee54))
* MET-1868 fix unit test ([d447233](https://github.com/cardano-foundation/cf-explorer-api/commit/d4472330873312045e7e1ec81f33463dafeef87b))
* MET-1868 optimize query get token of native script ([10f6e0b](https://github.com/cardano-foundation/cf-explorer-api/commit/10f6e0b5069d9c173ba6a4489641dc869047f390))
* MET-1868 update api get all and filter native script ([dbbd3e7](https://github.com/cardano-foundation/cf-explorer-api/commit/dbbd3e700988caef0888c7fd5e3a3ef3ffa614df))
* MET-1868 update api native script detail ([24defcf](https://github.com/cardano-foundation/cf-explorer-api/commit/24defcffa5de1732604807ca914ddd21c1d5e578))
* MET-2028 view current committee ([a430d81](https://github.com/cardano-foundation/cf-explorer-api/commit/a430d81f5c12b918638128f21617dbd26feafb33))
* MET-2116 use live query for total balance of address detail, stake address detail ([825ef9a](https://github.com/cardano-foundation/cf-explorer-api/commit/825ef9a8b5b414d7f8d32588eefb1de7b5b39dbe))
* MET-2215 add gov overview, filter api ([aa7fe12](https://github.com/cardano-foundation/cf-explorer-api/commit/aa7fe12c24b0418e22b50d6a7e51e49792772227))
* obtain range values and fix date filtering in DRep filter ([1b54614](https://github.com/cardano-foundation/cf-explorer-api/commit/1b546149cab59fc430b4b5f733c5f9385eaa4dfb))
* plutus v3 support ([5d71796](https://github.com/cardano-foundation/cf-explorer-api/commit/5d71796095e9fe4f58041137dee3421b6bb2c339))
* pools overview adjustments ([77e9e61](https://github.com/cardano-foundation/cf-explorer-api/commit/77e9e6115df262cebd474aeebe38c26bbf97b908))
* return governanceParticipationRate in response drep details ([8b68e98](https://github.com/cardano-foundation/cf-explorer-api/commit/8b68e98c1dab7757d271e32e53ab2ad17fd64151))
* return poolName ([d91ee27](https://github.com/cardano-foundation/cf-explorer-api/commit/d91ee279b8b6d9f1e0781cc68e0dd2fa5a925d9e))
* update AddressServiceImpl ([77d329f](https://github.com/cardano-foundation/cf-explorer-api/commit/77d329f62387b5abc420c70169cc1c69cab3310d))
* update gov action filter ([c2de28e](https://github.com/cardano-foundation/cf-explorer-api/commit/c2de28e3572155da3a82cc10c3f3d665682c9f7f))
* update GovernanceActionServiceImpl ([1f7d74f](https://github.com/cardano-foundation/cf-explorer-api/commit/1f7d74f1a16039f797d2f590809c683628f1d5c8))
* update ScriptServiceImpl to use two ls db-souce data ([23f9a6d](https://github.com/cardano-foundation/cf-explorer-api/commit/23f9a6d1b238fd44c3ed4190760a9bab2572224e))
* update stake endpoint when migrate with new ls schema ([bff777b](https://github.com/cardano-foundation/cf-explorer-api/commit/bff777bb41ad126eef0ba2d7372105ee368e8765))
* update TokenServiceImpl ([1e037f3](https://github.com/cardano-foundation/cf-explorer-api/commit/1e037f32202f8bd6408ab92378c2821f38877354))
* update TokenServiceImpl ([a36da00](https://github.com/cardano-foundation/cf-explorer-api/commit/a36da008fde3963460bf6576728eeca9b2a42974))
* update txs filter when migrate with new ls schema ([36f97b2](https://github.com/cardano-foundation/cf-explorer-api/commit/36f97b2b0b36d3dad9f7efdd05d9bd5daa91f27a))
* update votes section ([7f3d378](https://github.com/cardano-foundation/cf-explorer-api/commit/7f3d378056691c21047b45bd8842a9c7923bcb02))
* view individual cc member details ([711581c](https://github.com/cardano-foundation/cf-explorer-api/commit/711581c92f39c9fffc48b1705b0510afc2e2bae8))
* voting procedure chart ([caf1b83](https://github.com/cardano-foundation/cf-explorer-api/commit/caf1b83a9f053644dc0bd583443ee3a0500fbb68))


### Bug Fixes

* adapt code with ledger sync v2 ([3e089e0](https://github.com/cardano-foundation/cf-explorer-api/commit/3e089e007d5159282430cf10fbe663026c03e0f8))
* add createdOn field ([f9f15ef](https://github.com/cardano-foundation/cf-explorer-api/commit/f9f15efa2dd50524492ec1c286ca0b8704c79c90))
* add current timelock status in nativescript response ([37560a9](https://github.com/cardano-foundation/cf-explorer-api/commit/37560a9665e3c5f9b3a6e19a7ae81ea3d44ca25f))
* add default cip25 compliance value when metadata not pass ([f0bb884](https://github.com/cardano-foundation/cf-explorer-api/commit/f0bb88434a2dd918301763a0aa224323decdcda6))
* add missing field to kafka report message ([e515f1b](https://github.com/cardano-foundation/cf-explorer-api/commit/e515f1b097174011531922a7c7997805bd609048))
* add search token by address and fingerprint ([b86c313](https://github.com/cardano-foundation/cf-explorer-api/commit/b86c3139a830caa437893655015012903893aeaf))
* address balance chart incorrect ([332a2a3](https://github.com/cardano-foundation/cf-explorer-api/commit/332a2a38562608691248dfe404c42f1ee67f337e))
* can not filter by vote ([c9898f4](https://github.com/cardano-foundation/cf-explorer-api/commit/c9898f405a85bffafd6816fe36ee16d58d860b9a))
* can not filter pool by query when isShowRetired is null ([4e3e2eb](https://github.com/cardano-foundation/cf-explorer-api/commit/4e3e2eb47e4d4efa4bf65c63599d4dd0eca9ff28))
* can't filter by vote type NONE ([43c1916](https://github.com/cardano-foundation/cf-explorer-api/commit/43c191673c53c62a72173bd299f2cf58164f7575))
* can't get address balance of stake address ([d45788e](https://github.com/cardano-foundation/cf-explorer-api/commit/d45788e16c8ef13fa80ab00ccd9c68023eee8c8e))
* can't get list address of stake address ([277934f](https://github.com/cardano-foundation/cf-explorer-api/commit/277934fac38a47df76010f018b3da6fcb3bc36e9))
* can't get native script asset holder ([27bda99](https://github.com/cardano-foundation/cf-explorer-api/commit/27bda998fb17d42fe17c5e59b690111b99181a38))
* can't get stake wallet-activities ([82a40d9](https://github.com/cardano-foundation/cf-explorer-api/commit/82a40d99afaf946368d1c6660ecb9bf8f123d768))
* cannot get latest token activity time ([8053590](https://github.com/cardano-foundation/cf-explorer-api/commit/8053590152629d3dde4c0d8c2acc802d2277bf7a))
* cannot search DRep ([1befbe2](https://github.com/cardano-foundation/cf-explorer-api/commit/1befbe2da89c4f0e2c9f72f7313f6bcf1e6b5c71))
* cannot sort on gov participation rate column ([96302aa](https://github.com/cardano-foundation/cf-explorer-api/commit/96302aa4e357be854e22d769a0bc19fba623600c))
* cannot start app when firstShellyBlock not found ([034d314](https://github.com/cardano-foundation/cf-explorer-api/commit/034d314db1386e861105dae2a1366d81ce154897))
* catch Exception when reading json metadata ([fe5cbfa](https://github.com/cardano-foundation/cf-explorer-api/commit/fe5cbfa69f7e6de92179f956a30462d22d26be8e))
* check external api available ([8d671ea](https://github.com/cardano-foundation/cf-explorer-api/commit/8d671ea0796dc54c4e58fb3477fd61a8c9bbd088))
* checking for Bolnisi type “st”: “georgianWine” ([b31721b](https://github.com/cardano-foundation/cf-explorer-api/commit/b31721b3bff0a635d76eb45306b11b73cbe119f6))
* delegators of drep dont exactly ([5bb6135](https://github.com/cardano-foundation/cf-explorer-api/commit/5bb6135585960607aaaec315ebe26e3ff2e817fd))
* drep overview not handle case null status ([6324402](https://github.com/cardano-foundation/cf-explorer-api/commit/6324402571795034ceaaec0ec2c68c090bb5b30b))
* expiry date return null ([e486e5e](https://github.com/cardano-foundation/cf-explorer-api/commit/e486e5e5e7d795e5bdd79226e64f4329b0a3ce32))
* failed to sort several fields on pool list ([3ef63d0](https://github.com/cardano-foundation/cf-explorer-api/commit/3ef63d075e3de3778cb582b0a1508aad5562e2e4))
* fill wrong json property protocol param ([3c03d76](https://github.com/cardano-foundation/cf-explorer-api/commit/3c03d7602898f7849a3716f338c1b1e40cecb18f))
* fill zero TxChart time ([3ff74f5](https://github.com/cardano-foundation/cf-explorer-api/commit/3ff74f59167a278eaccab746df023944ad420e33))
* filter by CC in voting chart not working ([c64604a](https://github.com/cardano-foundation/cf-explorer-api/commit/c64604a95cad14000f138de3c70639bbfdc17487))
* filter by date-range in gov-action filter does not exactly work ([708cdb8](https://github.com/cardano-foundation/cf-explorer-api/commit/708cdb880436fc1a181f8c72b969533ae0adb069))
* filter by drep id and change pom version ([9c4cde4](https://github.com/cardano-foundation/cf-explorer-api/commit/9c4cde4526b04192ed1959d1036c471f8d9e3d23))
* filter by voting power is wrong ([add10bc](https://github.com/cardano-foundation/cf-explorer-api/commit/add10bce69d6059c83d1595831723c12ec08bb68))
* filter logic for pool ([9de004e](https://github.com/cardano-foundation/cf-explorer-api/commit/9de004eb4567518f8fb6a649e6b378a8e82c450d))
* find by drep id and recalculated chart ([51c59ff](https://github.com/cardano-foundation/cf-explorer-api/commit/51c59ff008e22a36c52f77b90ed6e264cc6cc908))
* fix logic and add filter for new column ([cf693ea](https://github.com/cardano-foundation/cf-explorer-api/commit/cf693eadd1cb1314f5b7e621ff38470a2dfb3ee9))
* fix logic query get native scripts ([274b04b](https://github.com/cardano-foundation/cf-explorer-api/commit/274b04b5a90f59cbac3ec52738faf3b2ee181fc1))
* force creating the next version ([0a91c3a](https://github.com/cardano-foundation/cf-explorer-api/commit/0a91c3a380a93d2392aeeb4361bbb9c9bfc593ef))
* genesis address not found in preprod|preview|sanchonet ([092f70b](https://github.com/cardano-foundation/cf-explorer-api/commit/092f70b361da965b1809802958a77906c4d24a5e))
* genesis tx thrown null pointer ([d5e96fd](https://github.com/cardano-foundation/cf-explorer-api/commit/d5e96fd8b6edf4aff3d51c64edc2fa7e7be9ce46))
* get delegators of DRep ([d263c79](https://github.com/cardano-foundation/cf-explorer-api/commit/d263c79e92b1fd3bb29e88600e4e0c5fd598df1f))
* get DReps by createdAt not exactly ([80ccc41](https://github.com/cardano-foundation/cf-explorer-api/commit/80ccc41bdefdeb1cf9ca737b0df4e14afe92e963))
* get token name not exactly ([03421b4](https://github.com/cardano-foundation/cf-explorer-api/commit/03421b46f724a5cf42335d4418becee2cc8a4930))
* gov action filter not avaiable ([0a42ac4](https://github.com/cardano-foundation/cf-explorer-api/commit/0a42ac4dd74af88a7c74749a46fe4bd8967a2a4a))
* gov filer error ([794ec22](https://github.com/cardano-foundation/cf-explorer-api/commit/794ec22186831ace89477d1c24d18b4f3b1fbcc9))
* gov filter error ([429cb99](https://github.com/cardano-foundation/cf-explorer-api/commit/429cb995c629b912b45a83c9bd714c5e6ee9e797))
* govActionLifeTime of epochParam return null ([732cf2e](https://github.com/cardano-foundation/cf-explorer-api/commit/732cf2e9dd8ef2caa7927939fa8c6355f36acc1a))
* handle case Public key API service is unavailable ([9399933](https://github.com/cardano-foundation/cf-explorer-api/commit/939993375cd868e61e138974a6b3dfef1c4032ef))
* handle status 500 server error ([d5df002](https://github.com/cardano-foundation/cf-explorer-api/commit/d5df002863a995a18dec96b57b23161d7c572069))
* issue with display gov action enum ([b45f002](https://github.com/cardano-foundation/cf-explorer-api/commit/b45f0021bce9146ff732b1ce5795bdc4b18e8f5d))
* MET-1858 update check native script is present ([cd45bae](https://github.com/cardano-foundation/cf-explorer-api/commit/cd45bae38189273037b0fb67fde58b380ae651ba))
* MET-1858 update check native script is present in native script asset holders and tokens ([682f6df](https://github.com/cardano-foundation/cf-explorer-api/commit/682f6dfee0da9bebb056670d7b319723d1e0b223))
* MET-1913 update postman collection sprint 0.9.0 ([6b324a6](https://github.com/cardano-foundation/cf-explorer-api/commit/6b324a6c99b2d4f75a78ba3e31ebecdb520f8b21))
* MET-1972 epoch status display incorrect while syncing ([ce89d98](https://github.com/cardano-foundation/cf-explorer-api/commit/ce89d988f1d8c065b69fab3f1aae29c813dc689c))
* missing condition when mapping AddressType ([6f9d65f](https://github.com/cardano-foundation/cf-explorer-api/commit/6f9d65f1a82a32131c4842990e521485456d6893))
* missing fingerprint when get token metadata in tx-mints ([9bb7c75](https://github.com/cardano-foundation/cf-explorer-api/commit/9bb7c75918ce725a0b1ac59e16f238cc252cc6a7))
* not map token_metadata when asset_metadata.subject is null ([51688c9](https://github.com/cardano-foundation/cf-explorer-api/commit/51688c93be01e42008f6717dc1b4cc686dbc0114))
* protocol param change timestamps not at epoch boudary ([ede3946](https://github.com/cardano-foundation/cf-explorer-api/commit/ede3946de22f718a502ddc926c554a5cbc58ca43))
* recalculate expiry date of gov action ([fdaaf32](https://github.com/cardano-foundation/cf-explorer-api/commit/fdaaf32ef7150519cf783b867d56aa0c06fa4519))
* recalculate gov participation rate of DReps ([4cfc695](https://github.com/cardano-foundation/cf-explorer-api/commit/4cfc695af418fc2abd494a3232a5d21dee0a2ed6))
* remove governanceParticipationRate that replaced with votingParticipation ([5c939e2](https://github.com/cardano-foundation/cf-explorer-api/commit/5c939e24a3bd9fa264e996c8ca55a3fe0adc07fa))
* remove previous market data cache ([e5448ff](https://github.com/cardano-foundation/cf-explorer-api/commit/e5448ff01aa600bb44057729bfb2cefd642c0b42))
* remove redundant annotation ([120c338](https://github.com/cardano-foundation/cf-explorer-api/commit/120c338a3f9762ac636a57989240862c5eb95ccf))
* replace logic check valid value in metadatacip25 ([e69117e](https://github.com/cardano-foundation/cf-explorer-api/commit/e69117e0b6471f2cbb56996ba3cd59c88ecf861e))
* replace logic get threshold for gov-action ([c643be1](https://github.com/cardano-foundation/cf-explorer-api/commit/c643be1508018be59fddbf5e87b368c32b54d7c1))
* return description of slot leader in block ([501d635](https://github.com/cardano-foundation/cf-explorer-api/commit/501d635d285ac782ba32c811b206c3c6c0ed0421))
* reward delegation sort is not correct ([88ce914](https://github.com/cardano-foundation/cf-explorer-api/commit/88ce914397b5fbc3f18e8e08a56f0a6d840ee7cb))
* sanchonet sc txs thrown 500 exceptions ([94c16ee](https://github.com/cardano-foundation/cf-explorer-api/commit/94c16ee31a6de1e31891f5e5e39a14f4f7d8261c))
* send wrong report message ([2645017](https://github.com/cardano-foundation/cf-explorer-api/commit/26450172cf8f6454cb72a274a419754612ab9cd4))
* set default value for conway genenis resource ([c7eb30d](https://github.com/cardano-foundation/cf-explorer-api/commit/c7eb30d4a9f829b44a5cb2bf2cb1b3c7e84454ed))
* sort failed ([c3ff1b2](https://github.com/cardano-foundation/cf-explorer-api/commit/c3ff1b27c2edf394fa7c87c430961557aae7da75))
* stake-lifecycle can't sort stake reward by time ([f4b90f7](https://github.com/cardano-foundation/cf-explorer-api/commit/f4b90f763cf43682b7df29a237334df7147e74f1))
* the value of saturation can be greater than 100 percent ([ae00722](https://github.com/cardano-foundation/cf-explorer-api/commit/ae00722756dbc55efd50fd139aa9d0d0da10057c))
* the voting participation rate is wrong ([f74de6a](https://github.com/cardano-foundation/cf-explorer-api/commit/f74de6ad3e7f525f5dac913bf1e25e00984c1e64))
* tx detail thrown null pointer ex ([9106304](https://github.com/cardano-foundation/cf-explorer-api/commit/9106304717faa50227eaff4967626b5556dff55f))
* update committee member in voting chart ([650cacf](https://github.com/cardano-foundation/cf-explorer-api/commit/650cacffc20b2a69eede5101b92841ad38b55443))
* update drep api ([839ae0a](https://github.com/cardano-foundation/cf-explorer-api/commit/839ae0adb38e6e0979cd0c63bfbb6583373377e9))
* update drep api ([9f579d0](https://github.com/cardano-foundation/cf-explorer-api/commit/9f579d0ce4c6a9d38a2e9e3e4a81329bb4deba2e))
* update gov action filter ([3a63a8a](https://github.com/cardano-foundation/cf-explorer-api/commit/3a63a8add413386994d3bc8550860baa4cf968ca))
* update it test for create proposal action ([98f63a6](https://github.com/cardano-foundation/cf-explorer-api/commit/98f63a667aacdb16d48e7003da90324ea833890c))
* update logic to get number of holder native script ([5e0b9f3](https://github.com/cardano-foundation/cf-explorer-api/commit/5e0b9f3e052430645abea67a467392563b41013a))
* update postman collection ([f058b5d](https://github.com/cardano-foundation/cf-explorer-api/commit/f058b5d9a494f335746a844b064c39b8b53579d3))
* update query of stake balance chart ([56ac961](https://github.com/cardano-foundation/cf-explorer-api/commit/56ac961bd0f3cb2039003d64f08ca48b593df9f6))
* update query of stake balance chart ([21bc3ce](https://github.com/cardano-foundation/cf-explorer-api/commit/21bc3ce8015d1fb761a1113d5c5353d1a0bf3917))
* update token_tx_count join query ([e9668e5](https://github.com/cardano-foundation/cf-explorer-api/commit/e9668e5ada463f4ca9af1eadc423119f58aa6360))
* update tx chart api ([cadff1e](https://github.com/cardano-foundation/cf-explorer-api/commit/cadff1e2864bd84e74331ad69f8b678b2389f2ee))
* update unit test ([f56ca32](https://github.com/cardano-foundation/cf-explorer-api/commit/f56ca329a6373e0fb843cdb9bec57edf9a12cfc9))
* vote type not valid ([aa931c4](https://github.com/cardano-foundation/cf-explorer-api/commit/aa931c46b4c2b5c9aab16ccb3c968b514719eeb7))
* voting procedure chart ([86f48b4](https://github.com/cardano-foundation/cf-explorer-api/commit/86f48b4687c5956780b4877a1dfc7a98d6d7209a))
* votingPower return null value instead of default value ([5acc575](https://github.com/cardano-foundation/cf-explorer-api/commit/5acc575c6a793c9ee7f4dde6d7b02912ff6a2521))
* where condition get pool list must be checked for null value ([9d3810a](https://github.com/cardano-foundation/cf-explorer-api/commit/9d3810af80dcff194c09b4646634d428f8f7e0b9))
* wrong active stake balance of gov ([756068f](https://github.com/cardano-foundation/cf-explorer-api/commit/756068f129e2b1a267c51ef29265194975a26cd8))
* wrong condition to get govs by drep ([ddf5a7c](https://github.com/cardano-foundation/cf-explorer-api/commit/ddf5a7c5dc7b323535c607471ae1d978c8bc37cd))
* wrong handle on error ([c0835b6](https://github.com/cardano-foundation/cf-explorer-api/commit/c0835b6375fd1f1ddf15e1c28d0f91114cbfc502))
* wrong mapping ([316a70b](https://github.com/cardano-foundation/cf-explorer-api/commit/316a70b842ff4f6c03f0b81249fb3e6fe2dccbf6))
* wrong slot in epoch ([2e8f011](https://github.com/cardano-foundation/cf-explorer-api/commit/2e8f01155d4958c0569151a3f65a460f7d620d22))
* wrong stake-tx balance change ([5b33d97](https://github.com/cardano-foundation/cf-explorer-api/commit/5b33d97b128250bf5c240f807027af19ae546a43))
* wrong token holders, can't not get setTotalBalanceOfPoolOwners ([58cc453](https://github.com/cardano-foundation/cf-explorer-api/commit/58cc45314babb14f85ab62615b0011ef14c8ded9))


### Performance Improvements

* change logic condition ([f539d9f](https://github.com/cardano-foundation/cf-explorer-api/commit/f539d9f2fbf867a1b8b255751b733b6850372423))
* improve perf native script list ([03d8b66](https://github.com/cardano-foundation/cf-explorer-api/commit/03d8b66986658c4df86412523647ed44304bcfd8))
* optimize perf script top-holder ([a0b1345](https://github.com/cardano-foundation/cf-explorer-api/commit/a0b134522c950d4c1601ecea4c6b5ede5888810e))
* optimize perf token filter list ([5276c37](https://github.com/cardano-foundation/cf-explorer-api/commit/5276c37353c4bf34a5b497497cb33beb1cc1deda))
* optimize perf top-address balance page ([17ccb48](https://github.com/cardano-foundation/cf-explorer-api/commit/17ccb4879521bd0a7a1f578e887d1521502f62fd))
* optimize perf tx filter list (by address, stake-address and token) ([5cbb8e5](https://github.com/cardano-foundation/cf-explorer-api/commit/5cbb8e5b89e7e095ff04ff0caa5b047cead7a023))
* reduce response time on details native script api ([f8c8024](https://github.com/cardano-foundation/cf-explorer-api/commit/f8c80247e577daa89796505577b4ff2b95447127))
* reduce time response when get voting chart of gov action by CC ([b9eced5](https://github.com/cardano-foundation/cf-explorer-api/commit/b9eced533feee6c05db4c2d244af631961010448))


### Miscellaneous Chores

* release 1.0.0 ([3a32a82](https://github.com/cardano-foundation/cf-explorer-api/commit/3a32a822dd13d561abe6a4dd5a838171226ce06d))
* release 1.1.0 ([f0adfd6](https://github.com/cardano-foundation/cf-explorer-api/commit/f0adfd6e078b8a6a17bed38fffe7cc25cede870f))
* release 1.2.0 ([47bed3a](https://github.com/cardano-foundation/cf-explorer-api/commit/47bed3a057679a2aa39e7a712818465add0096b9))
* release 1.2.0 ([8025de5](https://github.com/cardano-foundation/cf-explorer-api/commit/8025de5663101af7b365f2c9f2a6bbcce5fb788e))
* release 1.3.0 ([704a320](https://github.com/cardano-foundation/cf-explorer-api/commit/704a320ace4a6a392efa2e64feb6ddd037cb2de0))
* release 1.6.0 ([8ebcbbd](https://github.com/cardano-foundation/cf-explorer-api/commit/8ebcbbd5d1f8ad2f77fa14247dc0d88eebde2b20))

## [0.9.0](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.8.0...v0.9.0) (2024-03-05)


### Bug Fixes

* add createdOn field ([f9f15ef](https://github.com/cardano-foundation/cf-explorer-api/commit/f9f15efa2dd50524492ec1c286ca0b8704c79c90))
* failed to sort several fields on pool list ([3ef63d0](https://github.com/cardano-foundation/cf-explorer-api/commit/3ef63d075e3de3778cb582b0a1508aad5562e2e4))
* MET-1913 update postman collection sprint 0.9.0 ([6b324a6](https://github.com/cardano-foundation/cf-explorer-api/commit/6b324a6c99b2d4f75a78ba3e31ebecdb520f8b21))
* MET-1972 epoch status display incorrect while syncing ([ce89d98](https://github.com/cardano-foundation/cf-explorer-api/commit/ce89d988f1d8c065b69fab3f1aae29c813dc689c))
* remove previous market data cache ([e5448ff](https://github.com/cardano-foundation/cf-explorer-api/commit/e5448ff01aa600bb44057729bfb2cefd642c0b42))
* replace logic check valid value in metadatacip25 ([e69117e](https://github.com/cardano-foundation/cf-explorer-api/commit/e69117e0b6471f2cbb56996ba3cd59c88ecf861e))

## [0.8.0](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.7.0...v0.8.0) (2024-02-09)


### Bug Fixes

* add current timelock status in nativescript response ([37560a9](https://github.com/cardano-foundation/cf-explorer-api/commit/37560a9665e3c5f9b3a6e19a7ae81ea3d44ca25f))
* checking for Bolnisi type “st”: “georgianWine” ([b31721b](https://github.com/cardano-foundation/cf-explorer-api/commit/b31721b3bff0a635d76eb45306b11b73cbe119f6))
* handle case Public key API service is unavailable ([9399933](https://github.com/cardano-foundation/cf-explorer-api/commit/939993375cd868e61e138974a6b3dfef1c4032ef))
* handle status 500 server error ([d5df002](https://github.com/cardano-foundation/cf-explorer-api/commit/d5df002863a995a18dec96b57b23161d7c572069))
* missing fingerprint when get token metadata in tx-mints ([9bb7c75](https://github.com/cardano-foundation/cf-explorer-api/commit/9bb7c75918ce725a0b1ac59e16f238cc252cc6a7))
* not map token_metadata when asset_metadata.subject is null ([51688c9](https://github.com/cardano-foundation/cf-explorer-api/commit/51688c93be01e42008f6717dc1b4cc686dbc0114))
* update postman collection ([f058b5d](https://github.com/cardano-foundation/cf-explorer-api/commit/f058b5d9a494f335746a844b064c39b8b53579d3))
* wrong handle on error ([c0835b6](https://github.com/cardano-foundation/cf-explorer-api/commit/c0835b6375fd1f1ddf15e1c28d0f91114cbfc502))

## [0.7.0](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.6.0...v0.7.0) (2024-01-17)


### Bug Fixes

* force creating the next version ([0a91c3a](https://github.com/cardano-foundation/cf-explorer-api/commit/0a91c3a380a93d2392aeeb4361bbb9c9bfc593ef))

## [0.6.0](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.5.0...v0.6.0) (2024-01-17)


### Features

* check CIP-60 for json metadata ([0c69036](https://github.com/cardano-foundation/cf-explorer-api/commit/0c6903605a5fe3187178445d611ed40f7f01d3a2))
* MET-1740 fix test end2end ([be460b9](https://github.com/cardano-foundation/cf-explorer-api/commit/be460b9e795a921e1dba02be9d7d573accd4d204))
* MET-1740 show protocol parameters upcoming ([be3a34a](https://github.com/cardano-foundation/cf-explorer-api/commit/be3a34a113ee9e3935a6448550dd56bf27d7e0c1))
* pass with music version v1 or v2 ([37d4863](https://github.com/cardano-foundation/cf-explorer-api/commit/37d4863a8f50e0831d2910b64af18d8bc2325ba2))


### Bug Fixes

* conflict ([73632f6](https://github.com/cardano-foundation/cf-explorer-api/commit/73632f60b29b13dc83551bc0195d815496d39830))
* index of the properties ([b5bc133](https://github.com/cardano-foundation/cf-explorer-api/commit/b5bc133af22a6390e8099cac0f546626f83bf728))
* MET-1853 add pool hash raw to query in pool info ([73adb4f](https://github.com/cardano-foundation/cf-explorer-api/commit/73adb4fe8d475af340af38d548e1d6a2fdff9d96))
* no pass with music version v1,v2 ([91bb1c2](https://github.com/cardano-foundation/cf-explorer-api/commit/91bb1c26b28f9343c26f7d8b94f1f3419ac22211))
* output CIP-60 ([d18e1a2](https://github.com/cardano-foundation/cf-explorer-api/commit/d18e1a28627c0a0fdbe663372732b6ef5bbe8255))
* update logic get execution output of mint contract ([b0082d2](https://github.com/cardano-foundation/cf-explorer-api/commit/b0082d2eac4df7006a8e5512420372ded180991e))

## [0.5.0](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.35...v0.5.0) (2023-11-30)


### Features

* MET 1727 pool certificate history ([d4aaf19](https://github.com/cardano-foundation/cf-explorer-api/commit/d4aaf198a516f533591e030776f1d2b52afd85c2))
* MET 1739 multiple txs update protocol params ([5eca4b2](https://github.com/cardano-foundation/cf-explorer-api/commit/5eca4b2b42b5bd3f729e99c0e43059149e37210c))
* move metadata utils from common to api ([9c2edba](https://github.com/cardano-foundation/cf-explorer-api/commit/9c2edbaefb6a5c4ad41f31687a355fef313f0327))


### Bug Fixes

* current pool status is wrong ([4fb4435](https://github.com/cardano-foundation/cf-explorer-api/commit/4fb4435727fe7fcb7f99178d84b6bb9c0272ed75))
* define pool retired, reactive, update ([715aa39](https://github.com/cardano-foundation/cf-explorer-api/commit/715aa39fe5b0aab2cd6e378ff8f14ab69c08092d))
* edit package ([d473013](https://github.com/cardano-foundation/cf-explorer-api/commit/d4730136f469a32a95229ce7131fb66976669772))
* improve perf smart contract detail api ([23d2128](https://github.com/cardano-foundation/cf-explorer-api/commit/23d2128f26561f16d515fd1c5d78ccc4a1789369))
* MET-1805 update logic verify native script ([40ddf6e](https://github.com/cardano-foundation/cf-explorer-api/commit/40ddf6e3ddaceb91fff7aa49ae86088b31296188))
* MET-1819 update order of UTxOs in tx detail ([d0bbf8f](https://github.com/cardano-foundation/cf-explorer-api/commit/d0bbf8f8372009dc38676af506033750ab8bdcd1))
* missing genesis signer key for instantaneous reward txs ([b23b4de](https://github.com/cardano-foundation/cf-explorer-api/commit/b23b4dea55f40b877dced8a6b82168e09e16b10f))
* model package ([1281cee](https://github.com/cardano-foundation/cf-explorer-api/commit/1281cee07607e7d94e6eb9d2bc1e02e81e34303c))
* pool registration + update incorrect ([584b481](https://github.com/cardano-foundation/cf-explorer-api/commit/584b4818d741bf1b7b9e982c1c17e309b158cd8a))
* prepare the incorrect value for the genesis key in shelly genesis file ([c5a8503](https://github.com/cardano-foundation/cf-explorer-api/commit/c5a8503cf89662554c90c76b040851002d113f77))
* unit test ([56ca0db](https://github.com/cardano-foundation/cf-explorer-api/commit/56ca0db5a4a5b94e66c7731203ee3f23989ddfcb))
* update redis standalone and cluster to synchronize with other services ([2471018](https://github.com/cardano-foundation/cf-explorer-api/commit/24710180c39ded65123aa13cb148286e4d32dfee))

## [0.3.35](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.34...v0.3.35) (2023-11-08)


### Features

* MET-1742 add check native script and smart contract in policy detail ([f138725](https://github.com/cardano-foundation/cf-explorer-api/commit/f138725f357a29190bb6c097f78d4e4ae9e8def6))
* MET-1742 add check one time mint type of script ([dcd24b3](https://github.com/cardano-foundation/cf-explorer-api/commit/dcd24b3e10c2fdf96b777154342b13e801dea28f))
* MET-1742 add verify script api ([27685d6](https://github.com/cardano-foundation/cf-explorer-api/commit/27685d61ce7a1eb93307025edd4c7ae315ed68ed))
* MET-1742 check display mint burn policy only when native script verified ([9eba4a6](https://github.com/cardano-foundation/cf-explorer-api/commit/9eba4a620f18dd5e99974846293a9f7fbee21ea7))
* MET-1742 create native script detail and related api ([d949f6f](https://github.com/cardano-foundation/cf-explorer-api/commit/d949f6f33ebf5b138ca7996a83ddd4038135ea2f))
* MET-1746 script hash search ([17b1e50](https://github.com/cardano-foundation/cf-explorer-api/commit/17b1e5005c618fe5a08670d3c5c477ff79b6e4d5))


### Bug Fixes

* default sort not order by time ([e038a15](https://github.com/cardano-foundation/cf-explorer-api/commit/e038a1503d2fda4c1eb56fead1ec93cd8aae222c))
* MET-1742 add number tokens and number of asset holders to native script detail ([9ad2930](https://github.com/cardano-foundation/cf-explorer-api/commit/9ad2930dc338da5bd8af320614435e00bebd2ce9))
* MET-1742 count number asset holder in native script by policy by address and token ([9a79980](https://github.com/cardano-foundation/cf-explorer-api/commit/9a799800d55c7b7e88a61c66f493501eec77bf9e))
* MET-1742 rename verified in script ([dfbca12](https://github.com/cardano-foundation/cf-explorer-api/commit/dfbca12078d5dc2b8373f1f9493072b936034653))
* slice json metadata by asset name ([fca76f0](https://github.com/cardano-foundation/cf-explorer-api/commit/fca76f0fb7e9fe1fa6a70970713510266e32375e))
* verify script type belong to SC ([3f33c26](https://github.com/cardano-foundation/cf-explorer-api/commit/3f33c2612463c7af5bd88cb4b90fe5be51f9315e))

## [0.3.34](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.33...v0.3.34) (2023-10-30)


### Features

* [met-1514] implement sync-healthcheck endpoint ([32ce496](https://github.com/cardano-foundation/cf-explorer-api/commit/32ce49676614f9f518b4e3958b713e6b87394dd9))
* add custom actuator endpoint sync-status ([4558334](https://github.com/cardano-foundation/cf-explorer-api/commit/45583344384cdd638ce8ec2b9154aca0bb7a5ff9))
* add unauthorized exception ([a89436f](https://github.com/cardano-foundation/cf-explorer-api/commit/a89436f6ae544e5285c21ef61fc41334a1955555))
* add unauthorized exception ([dca3bed](https://github.com/cardano-foundation/cf-explorer-api/commit/dca3bed3cb78ec7fa896ccacbc589d57047092d9))
* dynamic author ([fdcf4f8](https://github.com/cardano-foundation/cf-explorer-api/commit/fdcf4f8d716d1febaea2534f9585421e697bd585))
* dynamic author ([588a5b5](https://github.com/cardano-foundation/cf-explorer-api/commit/588a5b5cbaa16827caf7d9294b3fe954a86a1194))
* handle signerKey in tx bootstrap witness ([7ca5803](https://github.com/cardano-foundation/cf-explorer-api/commit/7ca5803cf2d7d023656b888edfa4616c16f82ecd))
* map delegateKey to pkey hash with blake2b224 ([8a157ec](https://github.com/cardano-foundation/cf-explorer-api/commit/8a157ec3a38839b5dd47a256440429419963b43d))
* MET-1523 update postman collection fail asserts ([8d8e3da](https://github.com/cardano-foundation/cf-explorer-api/commit/8d8e3da6c2ccfb5facc961700a9278327b38e5a1))
* MET-1635 add filter for show or hide retired pool and add sort fields ([d7ada25](https://github.com/cardano-foundation/cf-explorer-api/commit/d7ada25925d85f7c5c8aaf403667b47928758cfa))
* MET-1666 handle null for fee in epoch list of pool detail ([25fbf82](https://github.com/cardano-foundation/cf-explorer-api/commit/25fbf8207b127bb9af2638372f2dc3c2ac50382a))
* met-1669 add signers info to protocol update response ([889aed3](https://github.com/cardano-foundation/cf-explorer-api/commit/889aed3696d088ad9a2dfc9deffe42d0af4a2562))
* MET-1700 use aggregated data for token detail and token list endpoint ([af5b8f9](https://github.com/cardano-foundation/cf-explorer-api/commit/af5b8f967de2448bb7b1b92b63a96b89d271c8f5))
* MET-1705 add signers information for MIR tx ([9d3fca4](https://github.com/cardano-foundation/cf-explorer-api/commit/9d3fca41839da13ff8081a8050ea1f1f4c8a82e4))
* MET-1706 add pool to reward list in stake lifecycle ([4b01dbe](https://github.com/cardano-foundation/cf-explorer-api/commit/4b01dbe1c358c84808ae8bb5cf0a8c3f72a34651))
* MET-1706 add total operator reward and total delegator reward to stake lifecycle ([33d2826](https://github.com/cardano-foundation/cf-explorer-api/commit/33d2826e0825aea9a36d79ff755cbc68c788005f))
* read config file from classpath ([c87518a](https://github.com/cardano-foundation/cf-explorer-api/commit/c87518acdb693e2fbc36e831904f5c0ea6fea25b))
* read config file from classpath ([a185413](https://github.com/cardano-foundation/cf-explorer-api/commit/a185413d364a4af256236ae593e631fa45bc8ebe))
* update report limit ([f20a03c](https://github.com/cardano-foundation/cf-explorer-api/commit/f20a03cd8654be062e60b5446f4fb69ae738de1a))
* update response of sync status ([c6c3362](https://github.com/cardano-foundation/cf-explorer-api/commit/c6c3362697844f28ca88f0192cad0d798f13f5a5))
* update unlimited report ([e5812e6](https://github.com/cardano-foundation/cf-explorer-api/commit/e5812e68dbf8cac593c6f19f4ec34f38a66a7191))
* update unlimited report ([aa66231](https://github.com/cardano-foundation/cf-explorer-api/commit/aa66231750e5698fb1a992ddc6bdc3d1bb4b8a6e))
* update unlimited report ([123f78e](https://github.com/cardano-foundation/cf-explorer-api/commit/123f78ee54741a2477ebb20599e3152a304692dc))
* update unlimited report ([cfc6219](https://github.com/cardano-foundation/cf-explorer-api/commit/cfc6219e1a740372d78c77bcdfeb4ae4101c602b))


### Bug Fixes

* change project index ([838f154](https://github.com/cardano-foundation/cf-explorer-api/commit/838f15408d95c8904cea5f29b2589eacb7e49ac7))
* conflict with develop ([9765e1c](https://github.com/cardano-foundation/cf-explorer-api/commit/9765e1cde83cccb234d2ef2128d0f1aba96029b4))
* edit field name ([3eb1ace](https://github.com/cardano-foundation/cf-explorer-api/commit/3eb1aceb0660f5fa621e17a6e7130d825a25a25c))
* MET-1706 update query to get pool data in reward ([6ece6e1](https://github.com/cardano-foundation/cf-explorer-api/commit/6ece6e1c5029b0922c8fd92c6fb21400fa829b3b))
* rename TxVkeyWitness to TxWitness ([04b0bc3](https://github.com/cardano-foundation/cf-explorer-api/commit/04b0bc338e8e594f2e15afead5e09faf6ca52ce1))
* update version common api ([63d3415](https://github.com/cardano-foundation/cf-explorer-api/commit/63d341591b70902cd45f66da86032a83195c3693))

## [0.3.33](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.32...v0.3.33) (2023-10-06)


### Features

* [MET-1640] Add poolName, poolId fields to the response when getting the list of delegation transactions for a stake key ([901ba69](https://github.com/cardano-foundation/cf-explorer-api/commit/901ba69d0a005eede1e4a7e4b47044f2dc827b73))
* add fixed-delay-time to publish market data to websocket ([6e04a37](https://github.com/cardano-foundation/cf-explorer-api/commit/6e04a37654b7cecefbd13affb165bc91d568140f))
* delete cache  protocol history ([28b037c](https://github.com/cardano-foundation/cf-explorer-api/commit/28b037c24cd06539aeb97bf3731a504178391ff2))
* impl redis-subscribe and websocket to publishing latest block to client ([d5b5f80](https://github.com/cardano-foundation/cf-explorer-api/commit/d5b5f805e18713e73bbd01dcc2978875d26e4a0a))
* MET-1533 rename coinPerUtxoSize to coinsPerByte ([59dff55](https://github.com/cardano-foundation/cf-explorer-api/commit/59dff5507dab957fd8d374fd6e86c9c29084f3e3))
* MET-1606 update tx contract response for new contract design page ([9715791](https://github.com/cardano-foundation/cf-explorer-api/commit/97157915464729d7938f629c4215b07a903ac5c8))
* MET-1664 limit result set when searching for token and pool ([efac65f](https://github.com/cardano-foundation/cf-explorer-api/commit/efac65fac0424a6137c0825cef2eca4c65935b53))
* MET-1672 handle missing genesis block data ([a719071](https://github.com/cardano-foundation/cf-explorer-api/commit/a7190713c5412735012711d24caf5e50ae1a8000))
* publish current epoch to websocket ([f7dca47](https://github.com/cardano-foundation/cf-explorer-api/commit/f7dca47a5beefdf0856f3e124ab8a1db0cb8cfe6))
* publish market price to websocket ([1687ac5](https://github.com/cardano-foundation/cf-explorer-api/commit/1687ac55e97c70c402c3fadb75d42b6e4d772acc))
* update cache fixed protocol ([edbe655](https://github.com/cardano-foundation/cf-explorer-api/commit/edbe6553560976877435f5767009a248e516ed31))


### Bug Fixes

* add datetime serializer for epoch summary ([7b42641](https://github.com/cardano-foundation/cf-explorer-api/commit/7b42641679809a7043b1a828c564b1c3e524fb22))
* add more information to websocket message ([ff44723](https://github.com/cardano-foundation/cf-explorer-api/commit/ff4472307de3dbbc405c188f4197679b58759a3e))
* add response token create date ([1523b7a](https://github.com/cardano-foundation/cf-explorer-api/commit/1523b7a30b28a0a4785af350925f0e63cb486f3b))
* add utxo hash and index to tx response ([a68f12f](https://github.com/cardano-foundation/cf-explorer-api/commit/a68f12f35f396d08fab7b5c23baeb438e2310bb6))
* change common method ([503eea2](https://github.com/cardano-foundation/cf-explorer-api/commit/503eea272c5dd492a84fccaf5c9daa269eb52675))
* change logic search to order by alphabet ([f80f1aa](https://github.com/cardano-foundation/cf-explorer-api/commit/f80f1aa91d7de1617f4d91f08732a0c699a1cd11))
* fetch reward distributed for Shelly block ([5f6b8da](https://github.com/cardano-foundation/cf-explorer-api/commit/5f6b8da0db4f02afa5ca1f8bc5d1d679a3b8ab25))
* fix typo KoiOs ([93f107a](https://github.com/cardano-foundation/cf-explorer-api/commit/93f107a3a0b0cb18c4452a97d00fb9a401385ff9))
* get data from genesis file using method common ([3f24fa0](https://github.com/cardano-foundation/cf-explorer-api/commit/3f24fa0102752e86aac1bdb699502bfa0881431d))
* handle case blockNo start with 0 ([6338029](https://github.com/cardano-foundation/cf-explorer-api/commit/63380296148ce97731626eb8bf35ae0f2c32aca7))
* handle connection exception to reward service ([9f57a4a](https://github.com/cardano-foundation/cf-explorer-api/commit/9f57a4a155f5d3a5657651aba20b44bfa28d89ab))
* MET-1528 change logic for analytic chart ([6eaecf2](https://github.com/cardano-foundation/cf-explorer-api/commit/6eaecf262af2beda6b7e6d6fdbaf44991df44460))
* MET-1559 update logic sorting when filter token and pool ([e1373f4](https://github.com/cardano-foundation/cf-explorer-api/commit/e1373f4d6ad1f1d114fc2e0556d297da33522748))
* MET-1629 missing rewards for epoch ([93eaed4](https://github.com/cardano-foundation/cf-explorer-api/commit/93eaed4dd88bffa094d76a9e9e7b14314da9e76f))
* MET-1651 add ticker name in pool list response ([631705a](https://github.com/cardano-foundation/cf-explorer-api/commit/631705a7f44d8442d6d26a3f8c8653e892d26a31))
* MET-1651 refactor code ([ba12ea4](https://github.com/cardano-foundation/cf-explorer-api/commit/ba12ea4a822022cd2bdccdfbcfa695aacc41281c))
* MET-1652 remove reward percentage in some pool response ([327ea0d](https://github.com/cardano-foundation/cf-explorer-api/commit/327ea0dca5defa64e1abb4b531369a5c8b3519b6))
* MET-1682 fix problem lazy fetch data ([4676167](https://github.com/cardano-foundation/cf-explorer-api/commit/4676167a261f68d46cc342763380ffa4151e7d31))
* MET-1682 remove transactional read only in service method ([d2af470](https://github.com/cardano-foundation/cf-explorer-api/commit/d2af4704fc4bf30407501ab358e18d1f4de01d4e))
* remove fetching pool history data from koios when getting pool list, top delegation pool, pool detail header ([6722d2e](https://github.com/cardano-foundation/cf-explorer-api/commit/6722d2e7cdfdd134ac8f58c4cd2e67b12289d639))
* resolved conflict with develop ([5390a25](https://github.com/cardano-foundation/cf-explorer-api/commit/5390a2599e23d20c7fce42e82862d53509465bdc))
* Retrieve parameter values from the genesis file instead of hardcode them ([31f5365](https://github.com/cardano-foundation/cf-explorer-api/commit/31f5365fc33789cf7817192d51c82fd1ca362e80))
* transaction detail protocol update error with cost model ([699c407](https://github.com/cardano-foundation/cf-explorer-api/commit/699c40729280b15de0fec8341ba2134be99e5904))
* update env genesis file ([15669ef](https://github.com/cardano-foundation/cf-explorer-api/commit/15669ef167c9caaed65d778a3cf0acf3d17c1102))
* update fill content from url and local file ([c9c22e2](https://github.com/cardano-foundation/cf-explorer-api/commit/c9c22e235703df898082843d3fe3b83db30fc68d))
* update flow websocket ([bb61299](https://github.com/cardano-foundation/cf-explorer-api/commit/bb61299ece67d1cc53650965a336c11f9d41123a))
* update logic pool search ([8e7d4f5](https://github.com/cardano-foundation/cf-explorer-api/commit/8e7d4f5a868ddc2fed7a92508a5213790ae034c0))
* update logic search all for token and pool when return one result ([c47bf76](https://github.com/cardano-foundation/cf-explorer-api/commit/c47bf7691e7655e22702552986eceba3b9e672f0))
* update logic to publish market data to ws ([25b9354](https://github.com/cardano-foundation/cf-explorer-api/commit/25b9354c9da9d7a5e67481875668542dc4852711))


### Performance Improvements

* improve performance block list info ([5b9fa3a](https://github.com/cardano-foundation/cf-explorer-api/commit/5b9fa3a299587d443c5868e4095bbb09416f7da4))

## [0.3.32](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.31...v0.3.32) (2023-08-28)


### Features

* change self-host to self-hosted ([878a01d](https://github.com/cardano-foundation/cf-explorer-api/commit/878a01d495e8817555426f8b34c5fa20541bc1a2))
* met-1480 setup for preview network ([4ff3b39](https://github.com/cardano-foundation/cf-explorer-api/commit/4ff3b392f577645a39454d3c3ab088c3c030ad2d))
* MET-1534 handle contract for failed tx ([8456f11](https://github.com/cardano-foundation/cf-explorer-api/commit/8456f117d2f9ca590f9c61b2640dda32633440ae))


### Bug Fixes

* Change another user to create report ([7d225d3](https://github.com/cardano-foundation/cf-explorer-api/commit/7d225d3b1ab8914da548a31ca1a8beb1a66fa315))
* delete item check & delete consolog ([e40fa5f](https://github.com/cardano-foundation/cf-explorer-api/commit/e40fa5fae645faea7b32da169c2f9f4151ebac0b))
* handle unauthorized user with pool report ([5d995c7](https://github.com/cardano-foundation/cf-explorer-api/commit/5d995c70b440dde1132fb5c203ce903af6448d76))
* update env ([8b2080b](https://github.com/cardano-foundation/cf-explorer-api/commit/8b2080b2ccd94d26935b53be7607a23e61fa199f))
* update postman test ([e8a8a32](https://github.com/cardano-foundation/cf-explorer-api/commit/e8a8a320624f7c72ac699eef5963bca6e8ca80c1))
* update Sign-in body ([4fca81a](https://github.com/cardano-foundation/cf-explorer-api/commit/4fca81a8acef472744897139192b8bdb268c4c08))


### Performance Improvements

* MET-1615 optimize list pool registration and de-registration ([77a175c](https://github.com/cardano-foundation/cf-explorer-api/commit/77a175ca1bb1b9867e3d47815fdff086073e6f4c))
* MET-1615 optimize list stake key registration and de-registration ([4dd4aad](https://github.com/cardano-foundation/cf-explorer-api/commit/4dd4aad4aeba1a9648b180d2d03559c974e957fc))

## [0.3.31](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.30...v0.3.31) (2023-08-14)


### Bug Fixes

* validate request not working ([13c9a9f](https://github.com/cardano-foundation/cf-explorer-api/commit/13c9a9ff776661bbdd6eb2fb3c6565f47ec9edf6))

## [0.3.30](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.29...v0.3.30) (2023-08-11)


### Features

* MET-1575 update logic to identify NFT or FT token type ([06e1f23](https://github.com/cardano-foundation/cf-explorer-api/commit/06e1f23653e6e4a54cf5fdcbe5e8db7521ced010))


### Bug Fixes

* MET-1563 change get top holders of token by unique account ([c4b7176](https://github.com/cardano-foundation/cf-explorer-api/commit/c4b7176c36f54c4cbe4d0a312731bc187bc66046))
* MET-1563 change query to check payment address and stake address ([c4b820c](https://github.com/cardano-foundation/cf-explorer-api/commit/c4b820c15bc8084d56a49177a88eb5a5aaea42a8))
* MET-1563 fix address type ([fed8e82](https://github.com/cardano-foundation/cf-explorer-api/commit/fed8e82b61237e4f3feef9480cffbfd6641b4616))
* MET-1563 fix unit test ([a1618a8](https://github.com/cardano-foundation/cf-explorer-api/commit/a1618a8c8071dba2d8846dc77a398ad6f49157f4))
* MET-1563 update response for stake and payment address ([788ddb0](https://github.com/cardano-foundation/cf-explorer-api/commit/788ddb00978b9495f50b166da7328157e60abe54))
* MET-1563 update unit test ([16de3d9](https://github.com/cardano-foundation/cf-explorer-api/commit/16de3d9a1af81ee3a33505985251a0bc0f38e646))

## [0.3.29](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.28...v0.3.29) (2023-08-09)


### Bug Fixes

* added kafka configuration ([5dbb357](https://github.com/cardano-foundation/cf-explorer-api/commit/5dbb3576e424d79edde54ee73de5fcd7e3872b23))
* update README.md and trigger release ([179a94c](https://github.com/cardano-foundation/cf-explorer-api/commit/179a94c45d0e6ce85a5e19810e9670a31d7e086f))

## [0.3.28](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.27...v0.3.28) (2023-08-08)


### Bug Fixes

* added string key serialiser ([89ce7f9](https://github.com/cardano-foundation/cf-explorer-api/commit/89ce7f959264571f79e7d8e421c44b4cd31b2149))

## [0.3.27](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.26...v0.3.27) (2023-08-07)


### Features

* MET-1530 check reward type of stake key ([e01c633](https://github.com/cardano-foundation/cf-explorer-api/commit/e01c63394f42e9e537421238e7e3c2d714585e47))
* update unit test for delegation and spo ([34dcc33](https://github.com/cardano-foundation/cf-explorer-api/commit/34dcc33da608d66715039730c27af454980fac24))


### Bug Fixes

* implement cip-25 to verify nft token metadata ([b4eca24](https://github.com/cardano-foundation/cf-explorer-api/commit/b4eca248276e75ab84f717c8f8424219e34972f7))
* MET_1420 change logic to get point for chart analytic ([a3b209e](https://github.com/cardano-foundation/cf-explorer-api/commit/a3b209e343ea30487accbd97d1b6fcbcd8edfc1f))
* met-1105 add time condition ([47664e2](https://github.com/cardano-foundation/cf-explorer-api/commit/47664e2ce496ad6f811a9d40d87b802c67333565))
* MET-1128 fix reward tab in stake lifecycle not active ([ce21094](https://github.com/cardano-foundation/cf-explorer-api/commit/ce21094ae96f2ef232e68f4189bb31f7adde9fdf))
* MET-1509 validate stake address in check stake address by payment address ([4a0f273](https://github.com/cardano-foundation/cf-explorer-api/commit/4a0f273dc72dac09e9583f93f053e6dfa88ef66a))
* met-684 fix search byron address not found ([bd8bce9](https://github.com/cardano-foundation/cf-explorer-api/commit/bd8bce90ac981836dee5c6a2fbf635f69ea06bb6))


### Documentation

* MET-1400 update swagger ([81b734f](https://github.com/cardano-foundation/cf-explorer-api/commit/81b734f8701f09ccd66e7efd90d347554ddbbcfe))

## [0.3.26](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.25...v0.3.26) (2023-08-02)


### Bug Fixes

* attempting to force a release ([a4f1536](https://github.com/cardano-foundation/cf-explorer-api/commit/a4f1536c9b9c753b893ad847e1d2b825aa87225a))

## [0.3.25](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.24...v0.3.25) (2023-08-01)


### Features

* MET-1128 remove unused reward type to query ([3f4c0cb](https://github.com/cardano-foundation/cf-explorer-api/commit/3f4c0cb11fbb4717bfc0f28437c72ebaa0c736e2))


### Bug Fixes

* add condition on findContract join statement query ([33e93b4](https://github.com/cardano-foundation/cf-explorer-api/commit/33e93b47724a6ece2f808a3199431d127d613c13))
* add logo endpoint in front of relative path logo ([226a58a](https://github.com/cardano-foundation/cf-explorer-api/commit/226a58ab090aa2e5770a99f5adb95f2d94438d41))

## [0.3.24](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.23...v0.3.24) (2023-07-27)


### Features

* MET-1407 update paging for api news ([bae75e8](https://github.com/cardano-foundation/cf-explorer-api/commit/bae75e8aed75b906fede497ae76e235e2b675587))

## [0.3.23](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.22...v0.3.23) (2023-07-25)


### Bug Fixes

* trim query search ([f5ff062](https://github.com/cardano-foundation/cf-explorer-api/commit/f5ff062abfe4b4016981cf3d8d617c390eaed29a))

## [0.3.22](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.21...v0.3.22) (2023-07-25)


### Bug Fixes

* change fee belongs to collateral when tx failed ([1223ca8](https://github.com/cardano-foundation/cf-explorer-api/commit/1223ca86ff4f87bf68bc48db744da9b95ad00949))
* fix unit test delegation header ([6f97ae6](https://github.com/cardano-foundation/cf-explorer-api/commit/6f97ae67e48fb49107b7065b41ffefcbfa276f6c))
* met-1394 handle kafka send report fail ([a1d2aa0](https://github.com/cardano-foundation/cf-explorer-api/commit/a1d2aa0269e404db8245458446db7983145b0cc3))

## [0.3.21](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.20...v0.3.21) (2023-07-20)


### Bug Fixes

* met-753 handle case field verified_contract null ([9e377aa](https://github.com/cardano-foundation/cf-explorer-api/commit/9e377aa527bde514af7f66f59e6932ef5411bbfc))


### Documentation

* add REPORT_LIMIT_PER_24HOURS env ([79694c3](https://github.com/cardano-foundation/cf-explorer-api/commit/79694c39f802313c55e42176a527adf0f8858c4f))

## [0.3.20](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.19...v0.3.20) (2023-07-19)


### Bug Fixes

* attempting to fix jedisConnectionFactory for cluster and standalone redis mode ([503f09b](https://github.com/cardano-foundation/cf-explorer-api/commit/503f09b65b979f48c7234140589c2a9dfdc2af7a))
* update API responses for improved coherence and logical consistency ([7b63621](https://github.com/cardano-foundation/cf-explorer-api/commit/7b63621692951b7056c6c5cf8be3680fba24bda6))

## [0.3.19](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.18...v0.3.19) (2023-07-17)


### Bug Fixes

* added redis cache TTL config for ada price ([26bb5ba](https://github.com/cardano-foundation/cf-explorer-api/commit/26bb5ba1c66ba68dc5f5ac1e8a8d36e6632faa9a))

## [0.3.18](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.17...v0.3.18) (2023-07-17)


### Features

* add unique account to epoch ([9a8a250](https://github.com/cardano-foundation/cf-explorer-api/commit/9a8a250e1cf0a291901067d26e281c516c0d3976))
* MET-1271 fix latest epoch detail error fetching reward ([ef5ec37](https://github.com/cardano-foundation/cf-explorer-api/commit/ef5ec3783a7134de930af711b52afe8f8721286f))
* MET-1271 fix unit test ([648ac59](https://github.com/cardano-foundation/cf-explorer-api/commit/648ac594e6e5f9835135a7d30f40206c9e0ed450))


### Bug Fixes

* conflict when create PR with develop ([e71ba52](https://github.com/cardano-foundation/cf-explorer-api/commit/e71ba527c4577a3127f3169e3a9a74e5d1e54297))
* fetch spo registration record is incorrect ([552e970](https://github.com/cardano-foundation/cf-explorer-api/commit/552e97031cc6edada2303b2602ddbf0737e3b85c))
* fix bug not return datum-out with same address of tx contract ([fe8662f](https://github.com/cardano-foundation/cf-explorer-api/commit/fe8662fca1e312ded408e8919adaff4e1bc3497d))
* handle case same address in contract detail ([0e1117a](https://github.com/cardano-foundation/cf-explorer-api/commit/0e1117a5e7e00325744c400eeefb0a6d13a3d8fb))
* miss code when fix conflict ([96008dc](https://github.com/cardano-foundation/cf-explorer-api/commit/96008dcd8a0aefe5f8927707890f3519a1ae8c7f))
* remove space in ts query tx_metadata ([53a4e2f](https://github.com/cardano-foundation/cf-explorer-api/commit/53a4e2f191d797801f91a3985d7e7a95a06a8991))
* reward distribution of pool not in epoch range ([58b8999](https://github.com/cardano-foundation/cf-explorer-api/commit/58b8999415847580c88d400f214f2f0da0db34c3))
* unit test pool certificate ([685f504](https://github.com/cardano-foundation/cf-explorer-api/commit/685f504029d4b7325308a3e984a54b5796c01816))
* update tx_contract_detail query after back fill redeemer,datum value ([eec72ca](https://github.com/cardano-foundation/cf-explorer-api/commit/eec72cacc362a150e68f2c3f262258615af16744))

## [0.3.17](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.16...v0.3.17) (2023-07-04)


### Bug Fixes

* update README.md trigger release ([5278c28](https://github.com/cardano-foundation/cf-explorer-api/commit/5278c28c5ed503146773e9845c06aeb1c0e977f9))

## [0.3.16](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.15...v0.3.16) (2023-07-03)


### Features

* change logic find top delegation by epoch block ([c69eb89](https://github.com/cardano-foundation/cf-explorer-api/commit/c69eb89bb964a94d002809941edc7f79241c3524))


### Bug Fixes

* add default sort pool certificate ([f5ba9e5](https://github.com/cardano-foundation/cf-explorer-api/commit/f5ba9e57504e25d201ac77c043afde5459534dc6))
* edit code logic pool deposit ([8acc448](https://github.com/cardano-foundation/cf-explorer-api/commit/8acc448822d57ca316bb184125bae203c7aa73f1))

## [0.3.15](https://github.com/cardano-foundation/cf-explorer-api/compare/v0.3.14...v0.3.15) (2023-06-30)


### Features

* get total delegators from cache ([86f774a](https://github.com/cardano-foundation/cf-explorer-api/commit/86f774af4ba47a45ab5f626a37dc9b6f58f2a963))


### Bug Fixes

* upgrade cumsumer common version ([a174119](https://github.com/cardano-foundation/cf-explorer-api/commit/a174119e486bc6366c85d008ef834db20c35c56c))

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
