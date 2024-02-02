package org.cardanofoundation.explorer.api.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.function.client.WebClient;

import com.google.gson.Gson;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.LotData;
import org.cardanofoundation.explorer.api.model.metadatastandard.bolnisi.MetadataBolnisi;
import org.cardanofoundation.explorer.api.repository.ledgersync.TxMetadataRepository;
import org.cardanofoundation.explorer.api.service.impl.BolnisiMetadataServiceImpl;
import org.cardanofoundation.explorer.consumercommon.entity.TxMetadata;
import org.cardanofoundation.ledgersync.common.util.HexUtil;

@ExtendWith(MockitoExtension.class)
class BolnisiMetadataServiceTest {

  private static final String BOLNISI_METADATA_KEY = "BOLNISI_METADATA:";
  final String network = "mainnet";
  final String offChainMetadataUrl =
      "https://offchain.pro.cf-bolnisi-mainnet.eu-west-1.bnwa.metadata.dev.cf-deployments.org/api/v1/storage/objectUrl/georgian-wine/{cid}";
  final String publicKeyUrl =
      "https://api.pro.cf-bolnisi-mainnet.eu-west-1.bnwa.metadata.dev.cf-deployments.org/api/v1/pubkeys/{wineryId}/v/0}";
  @Mock private TxMetadataRepository txMetadataRepository;
  @Mock private WebClient webClient;
  @Mock private WebClient.RequestHeadersSpec requestHeadersMock;
  @Mock private WebClient.RequestHeadersUriSpec requestHeadersUriMock;
  @Mock private WebClient.ResponseSpec responseMock;
  @Mock private RedisTemplate<String, Object> redisTemplate;
  @Mock private HashOperations hashOperations;

  @InjectMocks private BolnisiMetadataServiceImpl bolnisiMetadataService;

  @BeforeEach
  void setUp() {
    ReflectionTestUtils.setField(bolnisiMetadataService, "network", network);
    ReflectionTestUtils.setField(
        bolnisiMetadataService, "offChainMetadataUrl", offChainMetadataUrl);
    ReflectionTestUtils.setField(bolnisiMetadataService, "publicKeyUrl", publicKeyUrl);
  }

  @Test
  void
      getBolnisiMetadata_whenReadingOnChainMetadataThrownException_shouldReturnInvalidBolnisiMetadata() {
    String jsonMetadata =
        "{\"st\":\"georgianWine\",\"s\":[\"0xc5930c5cfac4e0ac457a9acd58d1eea7d48cd1672f9b3f849de2fdf9aac519f99df1e5cf4caf4f7813e1b73320ac9fac47f8ec821079eb6413d5db91fbcbb900\"],\"t\":\"conformityCert\",\"v\":\"1\",\"h\":\"0x7b22616c67223a224564445341227d\",\"pk\":\"0x19e50c2eb8c3a888a61b4d79163b057b7018075c67cd5755570d815de4c7dafe\",\"cid\":\"zCT5htke8aQRiirxnPtebY4SjpaMpCZzCnN7xMaA62KQdbYvw9V1\"}";
    MetadataBolnisi response = bolnisiMetadataService.getBolnisiMetadata(jsonMetadata);

    assertEquals("zCT5htke8aQRiirxnPtebY4SjpaMpCZzCnN7xMaA62KQdbYvw9V1", response.getCid());
    assertFalse(response.isCidVerified());
    assertFalse(response.isOnChainMetadataValid());
    assertTrue(response.isExternalApiAvailable());
  }

  @Test
  void getBolnisiMetadata_whenOnChainMetadataInvalid_shouldReturnInvalidBolnisiMetadata() {
    String jsonMetadata =
        "{\"st\":\"georgianWine\",\"s\":[\"0xc5930c5cfac4e0ac457a9acd58d1eea7d48cd1672f9b3f849de2fdf9aac519f99df1e5cf4caf4f7813e1b73320ac9fac47f8ec821079eb6413d5db91fbcbb900\"],\"t\":\"conformityCert\",\"v\":\"1\",\"h\":\"0x7b22616c67223a224564445341227d\",\"pk\":\"0x19e50c2eb8c3a888a61b4d79163b057b7018075c67cd5755570d815de4c7dafe\",\"cid\":\"zCT5htke8aQRiirxnPtebY4SjpaMpCZzCnN7xMaA62KQdbYvw9V1\"}";
    MetadataBolnisi response = bolnisiMetadataService.getBolnisiMetadata(jsonMetadata);

    assertEquals("zCT5htke8aQRiirxnPtebY4SjpaMpCZzCnN7xMaA62KQdbYvw9V1", response.getCid());
    assertFalse(response.isCidVerified());
    assertFalse(response.isOnChainMetadataValid());
    assertTrue(response.isExternalApiAvailable());
  }

  @Test
  void getBolnisiMetadata_whenExternalApiReturnEmpty_shouldReturnInvalidBolnisiMetadata() {
    String jsonMetadata =
        "{\"st\":\"georgianWine\",\"d\":{\"3\":{\"s\":[\"0xf91920ef8a5d0dd645d33d3e36d96fb6ca0f13963f02693a5494c10576c8591e7fef447e10bf96c8ee4ba690f3779801b5584d44c363dbfcaec550471cbff40c\"],\"h\":\"0x7b22616c67223a224564445341227d\",\"pk\":\"0x0e459119216db26ba7fec7a66462ed4502ef5bf5e25f447ce134e0acccf2be6f\"}},\"t\":\"scm\",\"v\":\"1\",\"cid\":\"\"}";
    MetadataBolnisi response = bolnisiMetadataService.getBolnisiMetadata(jsonMetadata);

    assertEquals("", response.getCid());
    assertFalse(response.isCidVerified());
  }

  @Test
  void getBolnisiMetadata_whenExternalApiNotAvailable_shouldReturnInvalidBolnisiMetadata() {
    String jsonMetadata =
        "{\"st\":\"georgianWine\",\"d\":{\"3\":{\"s\":[\"0xf91920ef8a5d0dd645d33d3e36d96fb6ca0f13963f02693a5494c10576c8591e7fef447e10bf96c8ee4ba690f3779801b5584d44c363dbfcaec550471cbff40c\"],\"h\":\"0x7b22616c67223a224564445341227d\",\"pk\":\"0x0e459119216db26ba7fec7a66462ed4502ef5bf5e25f447ce134e0acccf2be6f\"}},\"t\":\"scm\",\"v\":\"1\",\"cid\":\"zCT5htkeAx21JZxBPLWJ3ahSUkQUL2nfwKDpFBbFZ6XrZwVWmWaQ\"}";
    String offChainRedisKey = getRedisKey(BOLNISI_METADATA_KEY + offChainMetadataUrl);
    String actualOffChainDataUrl = "https://actual.off.chain.data.url";

    when(redisTemplate.opsForHash()).thenReturn(hashOperations);
    when(hashOperations.get(
            offChainRedisKey, "zCT5htkeAx21JZxBPLWJ3ahSUkQUL2nfwKDpFBbFZ6XrZwVWmWaQ"))
        .thenReturn(null);

    when(webClient.get()).thenReturn(requestHeadersUriMock);
    when(requestHeadersUriMock.uri(
            offChainMetadataUrl, "zCT5htkeAx21JZxBPLWJ3ahSUkQUL2nfwKDpFBbFZ6XrZwVWmWaQ"))
        .thenReturn(requestHeadersMock);
    when(requestHeadersUriMock.uri(actualOffChainDataUrl)).thenReturn(requestHeadersMock);

    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.onStatus(any(), any())).thenReturn(responseMock);
    when(responseMock.bodyToMono(String.class)).thenReturn(Mono.just(actualOffChainDataUrl));

    MetadataBolnisi response = bolnisiMetadataService.getBolnisiMetadata(jsonMetadata);
    assertFalse(response.isCidVerified());
    assertTrue(response.isOnChainMetadataValid());
    assertFalse(response.isExternalApiAvailable());
    assertNull(response.getWineryData());
  }

  @Test
  void getBolnisiMetadata_whenCidNotMatchWithOffChainData_shouldReturnInvalidBolnisiMetadata() {
    String jsonMetadata =
        "{\"st\":\"georgianWine\",\"d\":{\"3\":{\"s\":[\"0xf91920ef8a5d0dd645d33d3e36d96fb6ca0f13963f02693a5494c10576c8591e7fef447e10bf96c8ee4ba690f3779801b5584d44c363dbfcaec550471cbff40c\"],\"h\":\"0x7b22616c67223a224564445341227d\",\"pk\":\"0x0e459119216db26ba7fec7a66462ed4502ef5bf5e25f447ce134e0acccf2be6f\"}},\"t\":\"scm\",\"v\":\"1\",\"cid\":\"zCT5htkeAx21JZxBPLWJ3ahSUkQUL2nfwKDpFBbFZ6XrZwVWmWaQ\"}";
    String offChainRedisKey = getRedisKey(BOLNISI_METADATA_KEY + offChainMetadataUrl);

    when(redisTemplate.opsForHash()).thenReturn(hashOperations);
    when(hashOperations.get(
            offChainRedisKey, "zCT5htkeAx21JZxBPLWJ3ahSUkQUL2nfwKDpFBbFZ6XrZwVWmWaQ"))
        .thenReturn(getPreparedData());

    MetadataBolnisi response = bolnisiMetadataService.getBolnisiMetadata(jsonMetadata);
    assertEquals("zCT5htkeAx21JZxBPLWJ3ahSUkQUL2nfwKDpFBbFZ6XrZwVWmWaQ", response.getCid());
    assertFalse(response.isCidVerified());
    assertTrue(response.isExternalApiAvailable());
  }

  @Test
  void getBolnisiMetadata_whenAllDataValid_shouldReturnValidBolnisiMetadata() {
    String jsonMetadata =
        "{\"st\":\"georgianWine\",\"d\":{\"b\":{\"s\":[\"0xcba031e160496eba1d50709186008c30c85c9994c179fa85db3c3c19cbf3ed1d1015632ff1fcb3521af210217a0801d6a5cfe2f25e9806d54fed16ac00d2330d\",\"0x3fcd5f7cf8ec25a13710f14743794a62bfd974274c6d772ee38557435f5bbbc0ddd4e889888d67c1a630e329bec653d22091a666c5e3eb05057bf683a736180f\",\"0x98e19030d219f9e07ad3c5f2b9a2a90128beb1c8e4da127aa0acd6008ba7fd3f53bafcc608e9ab0df53b62d8d6cf7239aa334143668df72fd955ffb085809c0a\"],\"h\":\"0x7b22616c67223a224564445341227d\",\"pk\":\"0xd04938654a3540577f6721564da3471e62ca9da40fd7020b65ecaa9a9ff9a676\"}},\"t\":\"scm\",\"v\":\"1\",\"cid\":\"zCT5htke5RhzBU7mBTyDTvZgYajrkgzT4Eprkdxdrp9JtZYMHrbs\"}";
    String offChainRedisKey = getRedisKey(BOLNISI_METADATA_KEY + offChainMetadataUrl);

    when(redisTemplate.opsForHash()).thenReturn(hashOperations);
    when(hashOperations.get(
            offChainRedisKey, "zCT5htke5RhzBU7mBTyDTvZgYajrkgzT4Eprkdxdrp9JtZYMHrbs"))
        .thenReturn(getPreparedData());

    when(webClient.get()).thenReturn(requestHeadersUriMock);
    when(requestHeadersUriMock.uri(any(String.class), any(String.class)))
        .thenReturn(requestHeadersMock);

    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.onStatus(any(), any())).thenReturn(responseMock);

    when(responseMock.bodyToMono(byte[].class))
        .thenReturn(
            Mono.just(
                HexUtil.decodeHexString(
                    "0xd04938654a3540577f6721564da3471e62ca9da40fd7020b65ecaa9a9ff9a676")));

    MetadataBolnisi response = bolnisiMetadataService.getBolnisiMetadata(jsonMetadata);

    assertEquals("zCT5htke5RhzBU7mBTyDTvZgYajrkgzT4Eprkdxdrp9JtZYMHrbs", response.getCid());
    assertTrue(response.isCidVerified());
    assertTrue(response.isExternalApiAvailable());

    assertEquals(1, response.getWineryData().size());
    assertTrue(response.getWineryData().get(0).isPKeyVerified());
    assertEquals(3, response.getWineryData().get(0).getLots().size());

    assertEquals(
        "cba031e160496eba1d50709186008c30c85c9994c179fa85db3c3c19cbf3ed1d1015632ff1fcb3521af210217a0801d6a5cfe2f25e9806d54fed16ac00d2330d",
        response.getWineryData().get(0).getLots().get(0).getSignature());
    assertEquals(
        "3fcd5f7cf8ec25a13710f14743794a62bfd974274c6d772ee38557435f5bbbc0ddd4e889888d67c1a630e329bec653d22091a666c5e3eb05057bf683a736180f",
        response.getWineryData().get(0).getLots().get(1).getSignature());
    assertEquals(
        "98e19030d219f9e07ad3c5f2b9a2a90128beb1c8e4da127aa0acd6008ba7fd3f53bafcc608e9ab0df53b62d8d6cf7239aa334143668df72fd955ffb085809c0a",
        response.getWineryData().get(0).getLots().get(2).getSignature());
    assertTrue(
        response.getWineryData().get(0).getLots().stream().allMatch(LotData::isSignatureVerified));
  }

  @Test
  void getWineryData_shouldReturnWineryData() {
    String jsonMetadata =
        "{\"st\":\"georgianWine\",\"d\":{\"b\":{\"s\":[\"0xcba031e160496eba1d50709186008c30c85c9994c179fa85db3c3c19cbf3ed1d1015632ff1fcb3521af210217a0801d6a5cfe2f25e9806d54fed16ac00d2330d\",\"0x3fcd5f7cf8ec25a13710f14743794a62bfd974274c6d772ee38557435f5bbbc0ddd4e889888d67c1a630e329bec653d22091a666c5e3eb05057bf683a736180f\",\"0x98e19030d219f9e07ad3c5f2b9a2a90128beb1c8e4da127aa0acd6008ba7fd3f53bafcc608e9ab0df53b62d8d6cf7239aa334143668df72fd955ffb085809c0a\"],\"h\":\"0x7b22616c67223a224564445341227d\",\"pk\":\"0xd04938654a3540577f6721564da3471e62ca9da40fd7020b65ecaa9a9ff9a676\"}},\"t\":\"scm\",\"v\":\"1\",\"cid\":\"zCT5htke5RhzBU7mBTyDTvZgYajrkgzT4Eprkdxdrp9JtZYMHrbs\"}";
    String txHash = "69046f8d6510f888bf8b1682c6b29dd4d298dd478f9490abe0e0ee3e08b68e3d";
    String wineryId = "b";
    when(txMetadataRepository.findAllByTxHash(txHash))
        .thenReturn(
            List.of(TxMetadata.builder().key(BigInteger.valueOf(1904)).json(jsonMetadata).build()));

    String offChainRedisKey = getRedisKey(BOLNISI_METADATA_KEY + offChainMetadataUrl);

    when(redisTemplate.opsForHash()).thenReturn(hashOperations);
    when(hashOperations.get(
            offChainRedisKey, "zCT5htke5RhzBU7mBTyDTvZgYajrkgzT4Eprkdxdrp9JtZYMHrbs"))
        .thenReturn(getPreparedData());

    when(webClient.get()).thenReturn(requestHeadersUriMock);
    when(requestHeadersUriMock.uri(any(String.class), any(String.class)))
        .thenReturn(requestHeadersMock);

    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.onStatus(any(), any())).thenReturn(responseMock);

    when(responseMock.bodyToMono(byte[].class))
        .thenReturn(
            Mono.just(
                HexUtil.decodeHexString(
                    "0xd04938654a3540577f6721564da3471e62ca9da40fd7020b65ecaa9a9ff9a676")));

    var response = bolnisiMetadataService.getWineryData(txHash, wineryId);
    assertTrue(response.isPKeyVerified());
    assertEquals(3, response.getLots().size());

    assertEquals(
        "cba031e160496eba1d50709186008c30c85c9994c179fa85db3c3c19cbf3ed1d1015632ff1fcb3521af210217a0801d6a5cfe2f25e9806d54fed16ac00d2330d",
        response.getLots().get(0).getSignature());
    assertEquals(
        "3fcd5f7cf8ec25a13710f14743794a62bfd974274c6d772ee38557435f5bbbc0ddd4e889888d67c1a630e329bec653d22091a666c5e3eb05057bf683a736180f",
        response.getLots().get(1).getSignature());
    assertEquals(
        "98e19030d219f9e07ad3c5f2b9a2a90128beb1c8e4da127aa0acd6008ba7fd3f53bafcc608e9ab0df53b62d8d6cf7239aa334143668df72fd955ffb085809c0a",
        response.getLots().get(2).getSignature());
    assertTrue(response.getLots().stream().allMatch(LotData::isSignatureVerified));
  }

  private Map<String, List<Object>> getPreparedData() {
    String json =
        "{\"b\":[{\"bottling_date\":\"2023-08-20\",\"bottling_location\":\"Saint Peter and Paul Monastery, Bolnisi\",\"country_of_origin\":\"Georgia\",\"fermentation_duration\":\"10 Days\",\"fermentation_vessel\":\"Qvevri\",\"harvest_date\":\"2022-10-05\",\"harvest_location\":\"Bolnisi\",\"lot_number\":\"15643545800\",\"number_of_bottles\":5300,\"origin\":\"Bolnisi\",\"pressing_date\":\"2022-10-05\",\"processing_location\":\"Saint Peter and Paul Monastery, Bolnisi\",\"produced_by\":\"Zedashe\",\"producer_address\":\"Saint Peter and Paul Monastery, Bolnisi\",\"producer_latitude\":41.425843,\"producer_longitude\":44.539833,\"storage_vessel\":\"Stainless Steel Tanks\",\"varietal_name\":\"Saperavi\",\"vintage_year\":2022,\"wine_color\":\"Red\",\"wine_name\":\"Saperavi\",\"wine_type\":\"Dry\"},{\"bottling_date\":\"2023-10-22\",\"bottling_location\":\"Saint Peter and Paul Monastery, Bolnisi\",\"country_of_origin\":\"Georgia\",\"fermentation_duration\":\"15 Days\",\"fermentation_vessel\":\"Qvevri\",\"harvest_date\":\"2022-10-15\",\"harvest_location\":\"Asureti\",\"lot_number\":\"15643545802\",\"number_of_bottles\":530,\"origin\":\"Asureti\",\"pressing_date\":\"2022-10-15\",\"processing_location\":\"Saint Peter and Paul Monastery, Bolnisi\",\"produced_by\":\"Zedashe\",\"producer_address\":\"Saint Peter and Paul Monastery, Bolnisi\",\"producer_latitude\":41.425843,\"producer_longitude\":44.539833,\"storage_vessel\":\"Stainless Steel Tanks\",\"varietal_name\":\"Asuretuli Shala\",\"vintage_year\":2022,\"wine_color\":\"Red\",\"wine_name\":\"Asuretuli\",\"wine_type\":\"Dry\"},{\"bottling_date\":\"2023-08-07\",\"bottling_location\":\"Saint Peter and Paul Monastery, Bolnisi\",\"country_of_origin\":\"Georgia\",\"fermentation_duration\":\"15 Days\",\"fermentation_vessel\":\"Qvevri\",\"harvest_date\":\"2022-10-08\",\"harvest_location\":\"Bolnisi\",\"lot_number\":\"15643545817\",\"number_of_bottles\":1600,\"origin\":\"Bolnisi\",\"pressing_date\":\"2022-10-08\",\"processing_location\":\"Saint Peter and Paul Monastery, Bolnisi\",\"produced_by\":\"Zedashe\",\"producer_address\":\"Saint Peter and Paul Monastery, Bolnisi\",\"producer_latitude\":41.425843,\"producer_longitude\":44.539833,\"storage_vessel\":\"Stainless Steel Tanks\",\"varietal_name\":\"Shavkapito\",\"vintage_year\":2022,\"wine_color\":\"Red\",\"wine_name\":\"Shavkapito\",\"wine_type\":\"Dry\"}]}";
    return (Map<String, List<Object>>) new Gson().fromJson(json, Object.class);
  }

  private String getRedisKey(String value) {
    return String.format("%s_%s", network, value).toUpperCase();
  }
}
