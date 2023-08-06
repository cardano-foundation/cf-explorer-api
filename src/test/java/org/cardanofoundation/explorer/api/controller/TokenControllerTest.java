package org.cardanofoundation.explorer.api.controller;

import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutionException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.cardanofoundation.explorer.api.common.enumeration.AnalyticType;
import org.cardanofoundation.explorer.api.interceptor.AuthInterceptor;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.TxFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenAddressResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMetadataResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenMintTxResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenResponse;
import org.cardanofoundation.explorer.api.model.response.token.TokenVolumeAnalyticsResponse;
import org.cardanofoundation.explorer.api.service.TokenService;
import org.cardanofoundation.explorer.api.service.TxService;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

@WebMvcTest(TokenController.class)
@AutoConfigureMockMvc(addFilters = false)
class TokenControllerTest {

  @Autowired
  private MockMvc mockMvc;

  @MockBean
  private TokenService tokenService;

  @MockBean
  private TxService txService;

  @MockBean
  private AuthInterceptor authInterceptor;

  public static final String DATE_TIME_FORMAT = "yyyy/MM/dd HH:mm:ss";

  @BeforeEach
  void preControllerTest() throws Exception {
    when(authInterceptor.preHandle(any(), any(), any())).thenReturn(true);
  }

  @Test
  void testFilter() throws Exception {
    // Setup
    final TokenFilterResponse tokenFilterResponse = TokenFilterResponse.builder()
        .id(1746535L)
        .name("484f534b59")
        .displayName("HOSKY")
        .numberOfHolders(93233L)
        .volumeIn24h("38874867596070")
        .totalVolume("1276294773633720287")
        .fingerprint("asset17q7r59zlc3dgw0venc80pdv566q6yguw03f0d9")
        .txCount(5959388)
        .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
        .supply("1000000000000001")
        .createdOn(LocalDateTime.of(2021, 11, 5, 22, 15, 13))
        .build();

    final BaseFilterResponse<TokenFilterResponse> filter = new BaseFilterResponse<>(
        new PageImpl<>(List.of(tokenFilterResponse)));

    when(tokenService.filterToken(anyString(), any(Pageable.class)))
        .thenReturn(filter);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(get("/api/v1/tokens")
            .param("query", "HOSKY")
            .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals(asJsonString(filter), response.getContentAsString());
  }

  @Test
  void testFilter_WhenTokenServiceThrowsExecutionException() throws Exception {
    // Setup
    when(tokenService.filterToken(anyString(), any(Pageable.class)))
        .thenThrow(ExecutionException.class);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(get("/api/v1/tokens")
            .param("query", "query")
            .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), response.getStatus());
  }

  @Test
  void testFilter_WhenTokenServiceThrowsInterruptedException() throws Exception {
    // Setup
    when(tokenService.filterToken(anyString(), any(Pageable.class)))
        .thenThrow(InterruptedException.class);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(get("/api/v1/tokens")
            .param("query", "query")
            .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), response.getStatus());
  }

  @Test
  void testGetTokenDetail() throws Exception {
    // Setup
    final TokenResponse tokenResponse = new TokenResponse();
    tokenResponse.setName("484f534b59");
    tokenResponse.setDisplayName("HOSKY");
    tokenResponse.setPolicy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235");
    tokenResponse.setFingerprint("asset17q7r59zlc3dgw0venc80pdv566q6yguw03f0d9");
    tokenResponse.setTxCount(5959573);
    when(tokenService.getTokenDetail(anyString())).thenReturn(tokenResponse);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/{tokenId}", "asset17q7r59zlc3dgw0venc80pdv566q6yguw03f0d9")
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals(asJsonString(tokenResponse), response.getContentAsString());
  }

  @Test
  void testGetTokenMintTx() throws Exception {
    // Setup
    final TokenMintTxResponse tokenMintTxResponse = new TokenMintTxResponse();
    tokenMintTxResponse.setTxHash(
        "01b6f12e17c387fd0749674b331f63e1f5198ff6d651182c4b64826c487ce2a7");
    tokenMintTxResponse.setAmount("595990264");
    tokenMintTxResponse.setTime(LocalDateTime.of(2020, 1, 1, 0, 0, 0));

    final BaseFilterResponse<TokenMintTxResponse> filterResponse = new BaseFilterResponse<>(
        new PageImpl<>(List.of(tokenMintTxResponse)));
    when(tokenService.getMintTxs(anyString(), any(Pageable.class)))
        .thenReturn(filterResponse);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/{tokenId}/mints", "asset17q7r59zlc3dgw0venc80pdv566q6yguw03f0d9")
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals(asJsonString(filterResponse), response.getContentAsString());
  }

  @Test
  void testGetTopHolders() throws Exception {
    // Setup
    final TokenMetadataResponse metadataResponse = new TokenMetadataResponse();
    metadataResponse.setLogo("logo");
    metadataResponse.setUrl("url");
    metadataResponse.setDecimals(1000);
    metadataResponse.setTicker("ticker");

    final BaseFilterResponse<TokenAddressResponse> filterResponse = new BaseFilterResponse<>(
        new PageImpl<>(List.of(TokenAddressResponse.builder()
            .name("484f534b59")
            .displayName("HOSKY")
            .address("Ae2tdPwUPEYxA9ADeqSQkY9jAa9PJSTQVczRrVi68PsViqvdsWVLvVMJTQh")
            .policy("a0028f350aaabe0545fdcb56b039bfb08e4bb4d8c4d7c3c7d481c235")
            .quantity(BigInteger.valueOf(100L))
            .metadata(metadataResponse)
            .build())));
    when(tokenService.getTopHolders(anyString(), any(Pageable.class)))
        .thenReturn(filterResponse);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/{tokenId}/top_holders", "asset17q7r59zlc3dgw0venc80pdv566q6yguw03f0d9")
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals(asJsonString(filterResponse), response.getContentAsString());
  }

  @Test
  void testGetTransactions() throws Exception {
    // Setup
    final TxFilterResponse txFilterResponse = new TxFilterResponse();
    txFilterResponse.setId(0L);
    txFilterResponse.setHash("14cbfbf5c8f65d4085e76e7e8c8f53d4beaf749d02de1bcf77d6c95d8e7c347d");
    txFilterResponse.setBlockNo(10L);
    txFilterResponse.setBlockHash(
        "67f852c0d4214d8291e3c887c180b105c6cf859d79284de6aea4cdcfb186d4e5");
    txFilterResponse.setEpochNo(1000);

    final BaseFilterResponse<TxFilterResponse> filterResponse = new BaseFilterResponse<>(
        new PageImpl<>(List.of(txFilterResponse)));
    when(txService.getTransactionsByToken(anyString(), any(Pageable.class)))
        .thenReturn(filterResponse);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/{tokenId}/txs", "asset1tgpj6sgc33av38fdrnsry6s8p0zv7k5yv0xzam")
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals(asJsonString(filterResponse), response.getContentAsString());
  }

  @Test
  void testGetTokenVolumeAnalytics() throws Exception {
    // Setup
    final List<TokenVolumeAnalyticsResponse> tokenVolumeAnalyticsResponses = List.of(
        new TokenVolumeAnalyticsResponse(LocalDateTime.of(2020, 1, 1, 0, 0, 0),
            new BigInteger("100")));
    when(tokenService.getTokenVolumeAnalytic(anyString(), eq(AnalyticType.ONE_DAY)))
        .thenReturn(tokenVolumeAnalyticsResponses);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/analytics/{tokenId}/{type}",
                "asset1ccfemzcyzlmg9r6mu7hs92y97cxm3utavs93ld", AnalyticType.ONE_DAY)
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals(asJsonString(tokenVolumeAnalyticsResponses), response.getContentAsString());
  }

  @Test
  void testGetTokenVolumeAnalytics_WhenTokenServiceReturnsNoItems() throws Exception {
    // Setup
    when(tokenService.getTokenVolumeAnalytic("asset1nfetquxh86tjthzs6prsrstm8ts8gcpuxd8x0z",
        AnalyticType.ONE_DAY))
        .thenReturn(Collections.emptyList());

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/analytics/{tokenId}/{type}",
                "asset1nfetquxh86tjthzs6prsrstm8ts8gcpuxd8x0z", AnalyticType.ONE_DAY)
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.OK.value(), response.getStatus());
    assertEquals("[]", response.getContentAsString());
  }

  @Test
  void testGetTokenVolumeAnalytics_WhenTokenServiceThrowsExecutionException() throws Exception {
    // Setup
    when(tokenService.getTokenVolumeAnalytic(anyString(), eq(AnalyticType.ONE_DAY)))
        .thenThrow(ExecutionException.class);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/analytics/{tokenId}/{type}",
                "asset1nfetquxh86tjthzs6prsrstm8ts8gcpuxd8x0z", AnalyticType.ONE_DAY)
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), response.getStatus());
  }

  @Test
  void testGetTokenVolumeAnalytics_TokenServiceThrowsInterruptedException() throws Exception {
    // Setup
    when(tokenService.getTokenVolumeAnalytic(anyString(), eq(AnalyticType.ONE_DAY)))
        .thenThrow(InterruptedException.class);

    // Run the test
    final MockHttpServletResponse response = mockMvc.perform(
            get("/api/v1/tokens/analytics/{tokenId}/{type}",
                "asset1nfetquxh86tjthzs6prsrstm8ts8gcpuxd8x0z", AnalyticType.ONE_DAY)
                .accept(MediaType.APPLICATION_JSON))
        .andReturn().getResponse();

    // Verify the results
    assertEquals(HttpStatus.INTERNAL_SERVER_ERROR.value(), response.getStatus());
  }

  private static String asJsonString(final Object obj) {
    try {
      var objectMapper = new ObjectMapper();
      objectMapper.setDateFormat(new SimpleDateFormat(DATE_TIME_FORMAT));
      objectMapper.setSerializationInclusion(Include.NON_NULL);
      objectMapper.registerModule(new JavaTimeModule());
      return objectMapper.writeValueAsString(obj);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }
}
