package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.conversions.CardanoConverters;
import org.cardanofoundation.conversions.ClasspathConversionsFactory;
import org.cardanofoundation.conversions.domain.NetworkType;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.common.enumeration.TokenType;
import org.cardanofoundation.explorer.api.mapper.AssetMetadataMapper;
import org.cardanofoundation.explorer.api.mapper.MaTxMintMapper;
import org.cardanofoundation.explorer.api.mapper.TokenMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.*;
import org.cardanofoundation.explorer.api.projection.TokenProjection;
import org.cardanofoundation.explorer.api.repository.ledgersync.AssetMetadataRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MaTxMintRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.MultiAssetRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.ScriptRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AddressTxAmountRepository;
import org.cardanofoundation.explorer.api.repository.ledgersyncagg.AggregateAddressTokenRepository;
import org.cardanofoundation.explorer.api.service.cache.AggregatedDataCacheService;
import org.cardanofoundation.explorer.api.service.impl.TokenServiceImpl;
import org.cardanofoundation.explorer.common.entity.enumeration.ScriptType;
import org.cardanofoundation.explorer.common.entity.ledgersync.AssetMetadata;
import org.cardanofoundation.explorer.common.entity.ledgersync.MaTxMint;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
import org.cardanofoundation.explorer.common.entity.ledgersync.Script;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AggregateAddressToken;
import org.cardanofoundation.explorer.common.exception.BusinessException;

@ExtendWith(MockitoExtension.class)
class TokenServiceTest {

  @Mock private MultiAssetRepository multiAssetRepository;
  @Mock private MaTxMintRepository maTxMintRepository;
  @Mock private AssetMetadataRepository assetMetadataRepository;
  @Mock private AddressTxAmountRepository addressTxAmountRepository;
  @Mock private TokenMapper tokenMapper;
  @Mock private MaTxMintMapper maTxMintMapper;
  @Mock private AssetMetadataMapper assetMetadataMapper;
  @Mock private AggregateAddressTokenRepository aggregateAddressTokenRepository;

  @Mock private AggregatedDataCacheService aggregatedDataCacheService;
  @Mock private ScriptRepository scriptRepository;

  @InjectMocks private TokenServiceImpl tokenService;

  @Test
  void testFilterToken_WhenQueryEmpty() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(tokenService, "cardanoConverters", cardanoConverters);
    // Setup
    when(aggregatedDataCacheService.getTokenCount()).thenReturn(1);
    when(scriptRepository.findAllByHashIn(List.of("policy")))
        .thenReturn(List.of(Script.builder().type(ScriptType.TIMELOCK).hash("policy").build()));

    final TokenProjection tokenProjection = Mockito.mock(TokenProjection.class);
    when(tokenProjection.getPolicy()).thenReturn("policy");

    when(multiAssetRepository.findMultiAssets(any(Pageable.class)))
        .thenReturn(List.of(tokenProjection));
    final TokenFilterResponse tokenFilterResponse =
        TokenFilterResponse.builder()
            .id(0L)
            .name("name")
            .txCount(20)
            .unit("unit1")
            .policy("policy")
            .metadata(TokenMetadataResponse.builder().url("url").build())
            .build();

    when(assetMetadataMapper.fromTokenProjectionToTokenFilterResponse(tokenProjection))
        .thenReturn(tokenFilterResponse);

    // Run the test
    final BaseFilterResponse<TokenFilterResponse> result =
        tokenService.filterToken("", PageRequest.of(0, 1));

    // Verify the results
    assertEquals("url", result.getData().get(0).getMetadata().getUrl());
    assertEquals(20, result.getData().get(0).getTxCount());
  }

  @Test
  void testFilterToken_WhenQueryNotEmpty() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(tokenService, "cardanoConverters", cardanoConverters);
    // Setup
    when(aggregatedDataCacheService.getTokenCount()).thenReturn(1);
    TokenProjection tokenProjection = Mockito.mock(TokenProjection.class);
    when(tokenProjection.getPolicy()).thenReturn("policy");

    when(scriptRepository.findAllByHashIn(List.of("policy")))
        .thenReturn(List.of(Script.builder().type(ScriptType.TIMELOCK).hash("policy").build()));

    when(multiAssetRepository.findAll(anyString(), any(Pageable.class)))
        .thenReturn(List.of(tokenProjection));
    when(multiAssetRepository.countAllByQuery(anyString())).thenReturn(1L);

    final TokenFilterResponse tokenFilterResponse =
        TokenFilterResponse.builder()
            .unit("unit")
            .name("name")
            .policy("policy")
            .txCount(20)
            .fingerprint("fingerprint")
            .metadata(
                TokenMetadataResponse.builder()
                    .logo("logo")
                    .url("url")
                    .decimals(0)
                    .ticker("ticker")
                    .description("description")
                    .build())
            .build();

    when(assetMetadataMapper.fromTokenProjectionToTokenFilterResponse(tokenProjection))
        .thenReturn(tokenFilterResponse);

    // Run the test
    final BaseFilterResponse<TokenFilterResponse> result =
        tokenService.filterToken("name", PageRequest.of(0, 1));

    // Verify the results
    assertEquals("ticker", result.getData().get(0).getMetadata().getTicker());
    assertEquals(20, result.getData().get(0).getTxCount());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonIsNullAndTokenTypeIsFT() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset =
        MultiAsset.builder()
            .id(0L)
            .policy("d12d8c05c03484409f157917f21b323824d892130e4085006eaefc4a")
            .name("PARA0043")
            .nameView("PARA0043")
            .fingerprint("asset1kz0wkuzt8293x5jsz7tryyjdvs6mh7rcupf9nz")
            .unit("unit")
            .supply(BigInteger.ONE)
            .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    when(multiAssetRepository.getTokenTxCount(anyString())).thenReturn(Optional.of(15L));

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    final AssetMetadata metadata =
        AssetMetadata.builder()
            .id(0L)
            .subject("policyname")
            .name("name")
            .description("description")
            .policy("policy")
            .build();
    when(assetMetadataRepository.findFirstBySubject(any())).thenReturn(Optional.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse =
        new TokenMetadataResponse("url", "ticker", 0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(tokenService, "cardanoConverters", cardanoConverters);

    final long latestEpochTime = 1715133010;
    final LocalDateTime latestEpochDateTime =
        LocalDateTime.ofInstant(Instant.ofEpochSecond(latestEpochTime), ZoneOffset.UTC);
    final Timestamp latestTimestamp = Timestamp.valueOf(latestEpochDateTime);
    when(addressTxAmountRepository.getLastActivitySlotOfToken(multiAsset.getUnit()))
        .thenReturn(cardanoConverters.time().toSlot(latestEpochDateTime));

    when(maTxMintRepository.getTxMetadataToken(anyString(), any())).thenReturn(null);

    when(maTxMintRepository.getTxMetadataToken(anyString(), any())).thenReturn(null);

    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(TokenType.NFT, result.getTokenType());
    assertNull(result.getMetadataJson());
    assertEquals(latestTimestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
    assertEquals(15, result.getTxCount());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonNotContainVersionKeyAndTokenTypeIsNFT() {
    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(tokenService, "cardanoConverters", cardanoConverters);

    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset =
        MultiAsset.builder()
            .id(0L)
            .policy("0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884")
            .name("456c657068616e7453656372657441766174617273323135")
            .nameView("ElephantSecretAvatars215")
            .fingerprint("asset109mk0p5zlrk2sd5qt93v602v7zjuzax07dga47")
            .supply(BigInteger.ONE)
            .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    when(multiAssetRepository.getTokenTxCount(anyString())).thenReturn(Optional.of(20L));

    final AssetMetadata metadata =
        AssetMetadata.builder()
            .id(0L)
            .subject("policyname")
            .name("name")
            .description("description")
            .policy("policy")
            .build();
    when(assetMetadataRepository.findFirstBySubject(any())).thenReturn(Optional.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse =
        new TokenMetadataResponse("url", "ticker", 0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    final long latestEpochTime = 1715133010;
    final LocalDateTime latestEpochDateTime =
        LocalDateTime.ofInstant(Instant.ofEpochSecond(latestEpochTime), ZoneOffset.UTC);
    final Timestamp latestTimestamp = Timestamp.valueOf(latestEpochDateTime);
    when(addressTxAmountRepository.getLastActivitySlotOfToken(multiAsset.getUnit()))
        .thenReturn(cardanoConverters.time().toSlot(latestEpochDateTime));

    when(maTxMintRepository.getTxMetadataToken(anyString(), any()))
        .thenReturn(
            "{\"0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884\":{\"ElephantSecretAvatars215\":{\"image\":\"ipfs://QmNvjyj4o7p7UXMEbxnx9ZY5ZLFMJcy9sjMXaTiFRgC4nJ\",\"name\":\"ElephantSecretAvatars#0215\",\"files\":[{\"src\":\"ipfs://QmaRoAcJcHunUsEEE1FSiPm5SWe47PQbexwQpHZdbTLzde\",\"name\":\"ElephantSecretAvatars#0215\",\"mediaType\":\"model/gltf-binary\"}],\"Animation\":\"Idle\",\"Skin\":\"Pink\",\"mediaType\":\"image/png\"}}}");
    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(
        "{\"0495e7467b9f8285ef79fca99fe1ed85ca19faba5b7d4dd425c3d884\":{\"ElephantSecretAvatars215\":{\"image\":\"ipfs://QmNvjyj4o7p7UXMEbxnx9ZY5ZLFMJcy9sjMXaTiFRgC4nJ\",\"name\":\"ElephantSecretAvatars#0215\",\"files\":[{\"src\":\"ipfs://QmaRoAcJcHunUsEEE1FSiPm5SWe47PQbexwQpHZdbTLzde\",\"name\":\"ElephantSecretAvatars#0215\",\"mediaType\":\"model/gltf-binary\"}],\"Animation\":\"Idle\",\"Skin\":\"Pink\",\"mediaType\":\"image/png\"}}}",
        result.getMetadataJson());
    assertEquals(TokenType.NFT, result.getTokenType());
    assertEquals(latestTimestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
    assertEquals(20, result.getTxCount());
  }

  @Test
  void testGetTokenDetail_WhenTokenFoundAndMetadataJsonContainsVersionKeyAndTokenTypeIsNFT() {
    // Configure MultiAssetRepository.getLastActivityTimeOfToken(...).
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(tokenService, "cardanoConverters", cardanoConverters);

    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset =
        MultiAsset.builder()
            .id(0L)
            .policy("2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664")
            .name("50726f6d6973657332323339")
            .nameView("Promises2239")
            .fingerprint("asset1crku723fffqp4c6zmfmz8xc0cpwm6wugcwetmw")
            .supply(BigInteger.ONE)
            .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure TokenMapper.fromMultiAssetToResponse(...).
    final TokenResponse tokenResponse = new TokenResponse();
    when(tokenMapper.fromMultiAssetToResponse(any())).thenReturn(tokenResponse);

    when(multiAssetRepository.getTokenTxCount(anyString())).thenReturn(Optional.of(25L));

    final AssetMetadata metadata =
        AssetMetadata.builder()
            .id(0L)
            .subject("policyname")
            .name("name")
            .description("description")
            .policy("policy")
            .build();
    when(assetMetadataRepository.findFirstBySubject(any())).thenReturn(Optional.of(metadata));

    final TokenMetadataResponse tokenMetadataResponse =
        new TokenMetadataResponse("url", "ticker", 0, "logo", "description");
    when(assetMetadataMapper.fromAssetMetadata(metadata)).thenReturn(tokenMetadataResponse);

    final long latestEpochTime = 1715133010;
    final LocalDateTime latestEpochDateTime =
        LocalDateTime.ofInstant(Instant.ofEpochSecond(latestEpochTime), ZoneOffset.UTC);
    final Timestamp latestTimestamp = Timestamp.valueOf(latestEpochDateTime);
    when(addressTxAmountRepository.getLastActivitySlotOfToken(multiAsset.getUnit()))
        .thenReturn(cardanoConverters.time().toSlot(latestEpochDateTime));

    when(maTxMintRepository.getTxMetadataToken(anyString(), any()))
        .thenReturn(
            "{\"2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664\":{\"Promises2239\":{\"Candidate\":\"RichardTrixson\",\"image\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"Series\":\"CampaignMaterials\",\"Promise\":\"Correct\",\"Number\":\"2239\",\"Banner\":\"Modern\",\"Asset\":\"Promises2239\",\"files\":[{\"src\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"},{\"src\":\"ipfs://QmTWuebKD7FC8kKh8tBoPsVedyhJg38g92dBopnb7fM98C\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"}],\"Collection\":\"OldMoney\",\"mediaType\":\"image/jpeg\",\"Name\":\"Promises\"}},\"version\":\"1.0\"}");
    // Run the test
    final TokenResponse result = tokenService.getTokenDetail("tokenId");

    // Verify the results
    assertEquals(TokenType.NFT, result.getTokenType());
    assertEquals(
        "{\"2aec93fa65aaedaf2fc0aa46c3ace89c0c8e091ed5f39b8f8127e664\":{\"Promises2239\":{\"Candidate\":\"RichardTrixson\",\"image\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"Series\":\"CampaignMaterials\",\"Promise\":\"Correct\",\"Number\":\"2239\",\"Banner\":\"Modern\",\"Asset\":\"Promises2239\",\"files\":[{\"src\":\"ipfs://QmbWvwzLjwfKYqdbhar5KPzphAJQ1yZJ1xGbMi6C7A91CZ\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"},{\"src\":\"ipfs://QmTWuebKD7FC8kKh8tBoPsVedyhJg38g92dBopnb7fM98C\",\"name\":\"Promises2239\",\"mediaType\":\"image/jpeg\"}],\"Collection\":\"OldMoney\",\"mediaType\":\"image/jpeg\",\"Name\":\"Promises\"}},\"version\":\"1.0\"}",
        result.getMetadataJson());
    assertEquals(latestTimestamp, result.getTokenLastActivity());
    assertEquals(tokenMetadataResponse, result.getMetadata());
    assertEquals(25, result.getTxCount());
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
    when(maTxMintRepository.findByIdent(anyString(), any(Pageable.class))).thenReturn(maTxMints);

    // Configure MaTxMintMapper.fromMaTxMintToTokenMintTx(...).
    final TokenMintTxResponse tokenMintTxResponse = new TokenMintTxResponse();
    tokenMintTxResponse.setTxHash("txHash");
    tokenMintTxResponse.setAmount("amount");
    tokenMintTxResponse.setTime(LocalDateTime.of(2020, 1, 1, 0, 0, 0));

    when(maTxMintMapper.fromMaTxMintToTokenMintTx(maTxMint)).thenReturn(tokenMintTxResponse);

    // Run the test
    final BaseFilterResponse<TokenMintTxResponse> result =
        tokenService.getMintTxs("tokenId", PageRequest.of(0, 1));

    // Verify the results
    assertEquals(tokenMintTxResponse, result.getData().get(0));
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithOneDayRange() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(tokenService, "cardanoConverters", cardanoConverters);

    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset =
        MultiAsset.builder()
            .id(0L)
            .policy("policy")
            .name("name")
            .nameView("nameView")
            .unit("unit")
            .fingerprint("fingerprint")
            .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);
    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(addressTxAmountRepository.sumBalanceByUnitAndSlotBetween(anyString(), any(), any()))
        .thenReturn(Optional.of(new BigInteger("100")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result =
        tokenService.getTokenVolumeAnalytic("tokenId", AnalyticType.ONE_DAY);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(new BigInteger("100"), tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithOneDayRangeAndExistsZeroBalance() {
    CardanoConverters cardanoConverters =
        ClasspathConversionsFactory.createConverters(NetworkType.MAINNET);
    ReflectionTestUtils.setField(tokenService, "cardanoConverters", cardanoConverters);

    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset =
        MultiAsset.builder()
            .id(0L)
            .policy("policy")
            .name("name")
            .nameView("nameView")
            .unit("unit")
            .fingerprint("fingerprint")
            .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    // Configure AddressTokenRepository.sumBalanceBetweenTx(...).
    when(addressTxAmountRepository.sumBalanceByUnitAndSlotBetween(anyString(), any(), any()))
        .thenReturn(Optional.of(new BigInteger("0")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result =
        tokenService.getTokenVolumeAnalytic("tokenId", AnalyticType.ONE_DAY);

    // Verify the results
    for (TokenVolumeAnalyticsResponse tokenVolumeAnalyticsResponse : result) {
      assertEquals(BigInteger.ZERO, tokenVolumeAnalyticsResponse.getValue());
    }
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenFound_WithNotOneDayRange() {
    // Setup
    // Configure MultiAssetRepository.findByFingerprint(...).
    final MultiAsset multiAsset =
        MultiAsset.builder()
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
          AggregateAddressToken.builder()
              .balance(new BigInteger("100"))
              .day(LocalDate.now().minusDays(i))
              .build());
    }
    Collections.reverse(aggregateAddressTokens);
    when(aggregateAddressTokenRepository.findAllByUnitAndDayBetween(any(), any(), any()))
        .thenReturn(aggregateAddressTokens);

    lenient()
        .when(addressTxAmountRepository.sumBalanceByUnitAndSlotBetween(anyString(), any(), any()))
        .thenReturn(Optional.of(new BigInteger("100")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result =
        tokenService.getTokenVolumeAnalytic("tokenId", AnalyticType.ONE_WEEK);

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
    final MultiAsset multiAsset =
        MultiAsset.builder()
            .id(0L)
            .policy("policy")
            .name("name")
            .nameView("nameView")
            .unit("unit")
            .fingerprint("fingerprint")
            .build();

    final Optional<MultiAsset> multiAssetOpt = Optional.of(multiAsset);
    when(multiAssetRepository.findByFingerprint(anyString())).thenReturn(multiAssetOpt);

    lenient()
        .when(addressTxAmountRepository.sumBalanceByUnitAndSlotBetween(anyString(), any(), any()))
        .thenReturn(Optional.of(new BigInteger("0")));

    // Run the test
    final List<TokenVolumeAnalyticsResponse> result =
        tokenService.getTokenVolumeAnalytic("tokenId", AnalyticType.ONE_WEEK);

    // Verify the results
    assertEquals(new BigInteger("0"), result.get(result.size() - 1).getValue());
  }

  @Test
  void testGetTokenVolumeAnalytic_WhenTokenNotFound() {
    when(multiAssetRepository.findByFingerprint("tokenId")).thenReturn(Optional.empty());
    assertThrows(
        BusinessException.class,
        () -> tokenService.getTokenVolumeAnalytic("tokenId", AnalyticType.ONE_DAY));
  }
}
