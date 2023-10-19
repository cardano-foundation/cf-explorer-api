package org.cardanofoundation.explorer.api.service;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.TokenType;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenVolumeAnalyticsResponse;
import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.TokenProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTokenBalanceRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AggregateAddressTokenRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.StakeAddressRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.TokenInfoRepository;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
import org.cardanofoundation.explorer.api.service.impl.TokenServiceImpl;
import org.cardanofoundation.explorer.api.test.projection.AddressTokenProjectionImpl;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.common.exceptions.NoContentException;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;
import org.cardanofoundation.explorer.consumercommon.entity.TokenInfo;
import org.cardanofoundation.explorer.consumercommon.entity.aggregation.AggregateAddressToken;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(MockitoExtension.class)
class TokenServiceTest {

  @Mock
  private MultiAssetRepository multiAssetRepository;
  @Mock
  private MaTxMintRepository maTxMintRepository;
  @Mock
  private AssetMetadataRepository assetMetadataRepository;
  @Mock
  private AddressTokenRepository addressTokenRepository;
  @Mock
  private AddressRepository addressRepository;
  @Mock
  private AddressTokenBalanceRepository addressTokenBalanceRepository;
  @Mock
  private TokenInfoRepository tokenInfoRepository;
  @Mock
  private TokenMapper tokenMapper;
  @Mock
  private MaTxMintMapper maTxMintMapper;
  @Mock
  private AssetMetadataMapper assetMetadataMapper;
  @Mock
  private AggregateAddressTokenRepository aggregateAddressTokenRepository;

  @Mock
  private StakeAddressRepository stakeAddressRepository;
  @Mock
  private AggregatedDataCacheService aggregatedDataCacheService;

  @InjectMocks
  private TokenServiceImpl tokenService;

  @Test
  void testFilterToken_WhenQueryEmpty() {
    // Setup
    when(aggregatedDataCacheService.getTokenCount()).thenReturn(1);

    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    when(multiAssetRepository.findMultiAssets(any(Pageable.class))).thenReturn(
        List.of(multiAsset)
    );
    final TokenFilterResponse tokenFilterResponse = TokenFilterResponse.builder()
        .id(0L)
        .name("name")
        .policy("policy")
        .metadata(TokenMetadataResponse.builder().build())
        .build();

    when(tokenMapper.fromMultiAssetToFilterResponse(multiAsset))
        .thenReturn(tokenFilterResponse);

    final TokenInfo tokenInfo = TokenInfo.builder()
        .multiAssetId(0L)
        .numberOfHolders(100L)
        .volume24h(new BigInteger("100"))
        .updateTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();

    final List<TokenInfo> tokenInfos = List.of(tokenInfo);
    when(tokenInfoRepository.findTokenInfosByMultiAssetIdIn(anyCollection()))
        .thenReturn(tokenInfos);

    final AssetMetadata metadata = AssetMetadata.builder()
        .id(0L)
        .subject("policyname")
        .name("name")
        .description("description")
        .policy("policy")
        .build();
    when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(List.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");

    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Run the test
    final BaseFilterResponse<TokenFilterResponse> result = tokenService.filterToken(
        "", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(tokenMetadataResponse, result.getData().get(0).getMetadata());
    assertEquals("100", result.getData().get(0).getVolumeIn24h());
    assertEquals(100, result.getData().get(0).getNumberOfHolders());
  }

  @Test
  void testFilterToken_WhenQueryNotEmpty() {
    // Setup
    when(aggregatedDataCacheService.getTokenCount()).thenReturn(1);
    TokenProjection tokenProjection = Mockito.mock(TokenProjection.class);
    when(tokenProjection.getId()).thenReturn(0L);
    when(tokenProjection.getPolicy()).thenReturn("policy");
    when(tokenProjection.getName()).thenReturn("name");
    when(tokenProjection.getNameView()).thenReturn("nameView");
    when(tokenProjection.getFingerprint()).thenReturn("fingerprint");
    when(tokenProjection.getTxCount()).thenReturn(0L);
    when(tokenProjection.getSupply()).thenReturn(new BigInteger("0"));
    when(tokenProjection.getTotalVolume()).thenReturn(new BigInteger("0"));

    final Page<TokenProjection> multiAssets = new PageImpl<>(List.of(tokenProjection));

    when(multiAssetRepository.findAll(anyString(), any(Pageable.class))).thenReturn(
        multiAssets
    );

    final TokenFilterResponse tokenFilterResponse = TokenFilterResponse.builder()
        .id(0L)
        .name("name")
        .policy("policy")
        .metadata(TokenMetadataResponse.builder().build())
        .build();

    when(tokenMapper.fromMultiAssetToFilterResponse(any())).thenReturn(tokenFilterResponse);

    final TokenInfo tokenInfo = TokenInfo.builder()
        .multiAssetId(0L)
        .numberOfHolders(100L)
        .volume24h(new BigInteger("100"))
        .updateTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();

    final List<TokenInfo> tokenInfos = List.of(tokenInfo);
    when(tokenInfoRepository.findTokenInfosByMultiAssetIdIn(anyCollection()))
        .thenReturn(tokenInfos);

    final AssetMetadata metadata = AssetMetadata.builder()
        .id(0L)
        .subject("policyname")
        .name("name")
        .description("description")
        .policy("policy")
        .build();
    when(assetMetadataRepository.findBySubjectIn(any())).thenReturn(List.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");

    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Run the test
    final BaseFilterResponse<TokenFilterResponse> result = tokenService.filterToken(
        "name", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(tokenMetadataResponse, result.getData().get(0).getMetadata());
    assertEquals("100", result.getData().get(0).getVolumeIn24h());
    assertEquals(100, result.getData().get(0).getNumberOfHolders());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonIsNullAndTokenTypeIsFT() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("d12d8c05c03484409f157917f21b323824d892130e4085006eaefc4a")
        .name("PARA0043")
        .nameView("PARA0043")
        .fingerprint("asset1kz0wkuzt8293x5jsz7tryyjdvs6mh7rcupf9nz")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    final TokenInfo tokenInfo = TokenInfo.builder()
        .multiAssetId(0L)
        .numberOfHolders(100L)
        .volume24h(new BigInteger("100"))
        .updateTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();
    when(tokenInfoRepository.findTokenInfoByMultiAssetId(any())).thenReturn(Optional.of(tokenInfo));

    final AssetMetadata metadata = AssetMetadata.builder()
        .id(0L)
        .subject("policyname")
        .name("name")
        .description("description")
        .policy("policy")
        .build();
    when(assetMetadataRepository.findFirstBySubject(any())).thenReturn(Optional.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    final Timestamp timestamp = Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0));
    when(multiAssetRepository.getLastActivityTimeOfToken(multiAsset)).thenReturn(timestamp);

    when(maTxMintRepository.getTxMetadataNFTToken(anyString())).thenReturn(null);

    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(100, result.getNumberOfHolders());
    assertEquals("100", result.getVolumeIn24h());
    assertEquals(TokenType.FT, result.getTokenType());
    assertNull(result.getMetadataJson());
    assertEquals(timestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonNotContainVersionKeyAndTokenTypeIsNFT() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884")
        .name("456c657068616e7453656372657441766174617273323135")
        .nameView("ElephantSecretAvatars215")
        .fingerprint("asset109mk0p5zlrk2sd5qt93v602v7zjuzax07dga47")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    final TokenInfo tokenInfo = TokenInfo.builder()
        .multiAssetId(0L)
        .numberOfHolders(100L)
        .volume24h(new BigInteger("100"))
        .updateTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();
    when(tokenInfoRepository.findTokenInfoByMultiAssetId(any())).thenReturn(Optional.of(tokenInfo));

    final AssetMetadata metadata = AssetMetadata.builder()
        .id(0L)
        .subject("policyname")
        .name("name")
        .description("description")
        .policy("policy")
        .build();
    when(assetMetadataRepository.findFirstBySubject(any())).thenReturn(Optional.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    final Timestamp timestamp = Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0));
    when(multiAssetRepository.getLastActivityTimeOfToken(multiAsset)).thenReturn(timestamp);

    when(maTxMintRepository.getTxMetadataNFTToken(anyString())).thenReturn(
        "{\"0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884\":{\"ElephantSecretAvatars215\":{\"image\":\"ipfs://QmNvjyj4o7p7UXMEbxnx9ZY5ZLFMJcy9sjMXaTiFRgC4nJ\",\"name\":\"ElephantSecretAvatars#0215\",\"files\":[{\"src\":\"ipfs://QmaRoAcJcHunUsEEE1FSiPm5SWe47PQbexwQpHZdbTLzde\",\"name\":\"ElephantSecretAvatars#0215\",\"mediaType\":\"model/gltf-binary\"}],\"Animation\":\"Idle\",\"Skin\":\"Pink\",\"mediaType\":\"image/png\"}}}");
    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(100, result.getNumberOfHolders());
    assertEquals("100", result.getVolumeIn24h());
    assertEquals(
        "{\"0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884\":{\"ElephantSecretAvatars215\":{\"image\":\"ipfs://QmNvjyj4o7p7UXMEbxnx9ZY5ZLFMJcy9sjMXaTiFRgC4nJ\",\"name\":\"ElephantSecretAvatars#0215\",\"files\":[{\"src\":\"ipfs://QmaRoAcJcHunUsEEE1FSiPm5SWe47PQbexwQpHZdbTLzde\",\"name\":\"ElephantSecretAvatars#0215\",\"mediaType\":\"model/gltf-binary\"}],\"Animation\":\"Idle\",\"Skin\":\"Pink\",\"mediaType\":\"image/png\"}}}",
        result.getMetadataJson());
    assertEquals(TokenType.NFT, result.getTokenType());
    assertEquals(timestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonContainsVersionKeyAndTokenTypeIsNFT() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664")
        .name("50726f6d6973657332323339")
        .nameView("Promises2239")
        .fingerprint("asset1crku723fffqp4c6zmfmz8xc0cpwm6wugcwetmw")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    final TokenInfo tokenInfo = TokenInfo.builder()
        .multiAssetId(0L)
        .numberOfHolders(100L)
        .volume24h(new BigInteger("100"))
        .updateTime(Timestamp.valueOf(LocalDateTime.now()))
        .build();
    when(tokenInfoRepository.findTokenInfoByMultiAssetId(any())).thenReturn(Optional.of(tokenInfo));

    final AssetMetadata metadata = AssetMetadata.builder()
        .id(0L)
        .subject("policyname")
        .name("name")
        .description("description")
        .policy("policy")
        .build();
    when(assetMetadataRepository.findFirstBySubject(any())).thenReturn(Optional.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse = new TokenMetadataResponse("url", "ticker",
        0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    final Timestamp timestamp = Timestamp.valueOf(LocalDateTime.of(2020, 1, 1, 0, 0, 0, 0));
    when(multiAssetRepository.getLastActivityTimeOfToken(multiAsset)).thenReturn(timestamp);

    when(maTxMintRepository.getTxMetadataNFTToken(anyString())).thenReturn(
        "{\"2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664\":{\"Promises2239\":{\"Candidate\":\"RichardTrixson\",\"image\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"Series\":\"CampaignMaterials\",\"Promise\":\"Correct\",\"Number\":\"2239\",\"Banner\":\"Modern\",\"Asset\":\"Promises2239\",\"files\":[{\"src\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"},{\"src\":\"ipfs://QmTWuebKD7FC8kKh8tBoPsVedyhJg38g92dBopnb7fM98C\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"}],\"Collection\":\"OldMoney\",\"mediaType\":\"image/jpeg\",\"Name\":\"Promises\"}},\"version\":\"1.0\"}");
    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(100, result.getNumberOfHolders());
    assertEquals("100", result.getVolumeIn24h());
    assertEquals(TokenType.NFT, result.getTokenType());
    assertEquals(
        "{\"2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664\":{\"Promises2239\":{\"Candidate\":\"RichardTrixson\",\"image\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"Series\":\"CampaignMaterials\",\"Promise\":\"Correct\",\"Number\":\"2239\",\"Banner\":\"Modern\",\"Asset\":\"Promises2239\",\"files\":[{\"src\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"},{\"src\":\"ipfs://QmTWuebKD7FC8kKh8tBoPsVedyhJg38g92dBopnb7fM98C\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"}],\"Collection\":\"OldMoney\",\"mediaType\":\"image/jpeg\",\"Name\":\"Promises\"}},\"version\":\"1.0\"}",
        result.getMetadataJson());
    assertEquals(timestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
  }

  @Test
  void testGetTokenDetail_WhenTokenNotFound() {
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(Optional.empty());
    assertThrows(BusinessException.class, () -> tokenService.getTokenDetail("tokenId"));
  }

  @Test
  void testGetMintTxs() {
    // Setup
    // Configure MaTxMintRepository.findByIdent(...).
    final MaTxMint maTxMint = new MaTxMint();
    maTxMint.setId(0L);
    final MultiAsset ident = new MultiAsset();
    ident.setPolicy("policy");
    ident.setName("name");
    ident.setNameView("nameView");
    maTxMint.setIdent(ident);
    final Page<MaTxMint> maTxMints = new PageImpl<>(List.of(maTxMint));
    when(maTxMintRepository.findByIdent(anyString(), any(Pageable.class)))
        .thenReturn(maTxMints);

    // Configure MaTxMintMapper.fromMaTxMintToTokenMintTx(...).
    final TokenMintTxResponse tokenMintTxResponse = new TokenMintTxResponse();
    tokenMintTxResponse.setTxHash("txHash");
    tokenMintTxResponse.setAmount("amount");
    tokenMintTxResponse.setTime(LocalDateTime.of(2020, 1, 1, 0, 0, 0));

    when(maTxMintMapper.fromMaTxMintToTokenMintTx(maTxMint)).thenReturn(tokenMintTxResponse);

    // Run the test
    final BaseFilterResponse<TokenMintTxResponse> result = tokenService.getMintTxs(
        "tokenId", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(tokenMintTxResponse, result.getData().get(0));
  }

  @Test
  void testGetTopHolders_WhenTokenFound() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure AddressTokenBalanceRepository.findAddressAndBalanceByMultiAsset(...).
    when(addressTokenBalanceRepository.findAddressAndBalanceByMultiAsset(eq(multiAsset),
        any(Pageable.class))).thenReturn(List.of(
        AddressTokenProjectionImpl
            .builder()
            .addressId(1L)
            .build()
    ));

    // Configure AddressRepository.findAddressByIdIn(...).
    final Address address = Address.builder()
        .id(0L)
        .address("address")
        .txCount(10L)
        .balance(new BigInteger("100"))
        .build();
    final List<Address> addresses = List.of(address);
    when(addressRepository.findAddressByIdIn(anyCollection())).thenReturn(addresses);

    // Configure TokenMapper.fromAddressTokenProjection(...).
    final TokenAddressResponse tokenAddressResponse = TokenAddressResponse.builder()
        .address(address.getAddress())
        .addressId(address.getId())
        .build();
    final StakeAddress stakeAddress = StakeAddress.builder()
        .id(0L)
        .view("address")
        .build();
    when(stakeAddressRepository.findByIdIn(anyCollection())).thenReturn(List.of(stakeAddress));

    when(tokenMapper.fromAddressTokenProjection(any(AddressTokenProjection.class)))
        .thenReturn(tokenAddressResponse);

    // Run the test
    final BaseFilterResponse<TokenAddressResponse> result = tokenService.getTopHolders(
        "tokenId", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(address.getAddress(), result.getData().get(0).getAddress());
  }

  @Test
  void testGetTopHolders_WhenTokenNotFound() {
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(Optional.empty());
    assertThrows(NoContentException.class,
        () -> tokenService.getTopHolders("tokenId", PageRequest.of(0, 1)));
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithOneDayRange() throws Exception {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);
    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
        any(),
        any()))
        .thenReturn(Optional.of(new BigInteger("100")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_DAY);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(new BigInteger("100"), tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithOneDayRangeAndExistsZeroBalance() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
        any(),
        any()))
        .thenReturn(Optional.of(new BigInteger("0")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_DAY);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(BigInteger.ZERO, tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithNotOneDayRange() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);
    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).

    List<AggregateAddressToken> aggregateAddressTokens = new ArrayList<>();
    for (int i = 0; i < 10; i++) {
      aggregateAddressTokens.add(
          new AggregateAddressToken(0L, new BigInteger("100"), LocalDate.now().minusDays(i))
      );
    }
    Collections.reverse(aggregateAddressTokens);
    when(aggregateAddressTokenRepository
        .findAllByIdentAndDayBetween(any(), any(), any())).thenReturn(aggregateAddressTokens);

    lenient().when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
            any(),
            any()))
        .thenReturn(Optional.of(new BigInteger("100")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_WEEK);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(new BigInteger("100"), tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithNotOneDayRangeAndExistsZeroBalance()
      throws Exception {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset = MultiAsset.builder()
        .id(0L)
        .policy("policy")
        .name("name")
        .nameView("nameView")
        .fingerprint("fingerprint")
        .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);


    lenient().when(addressTokenRepository.sumBalanceBetweenTx(any(MultiAsset.class),
            any(),
            any()))
        .thenReturn(Optional.of(new BigInteger("0")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result = tokenService.getTokenVolumeAnalytic(
        "tokenId", AnalyticType.ONE_WEEK);

    // Verify the results
    assertEquals(new BigInteger("0"), result.get(result.size() - 1).getValue());
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenNotFound() {
    when(multiAssetRepository.findByFingerprint("tokenId")).thenReturn(Optional.empty());
    assertThrows(NoContentException.class,
        () -> tokenService.getTokenVolumeAnalytic("tokenId", AnalyticType.ONE_DAY));
  }
}