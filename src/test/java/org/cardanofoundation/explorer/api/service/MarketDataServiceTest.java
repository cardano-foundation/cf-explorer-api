package org.cardanofoundation.explorer.api.service;

import java.nio.charset.StandardCharsets;
import java.util.function.Function;
import java.util.function.Predicate;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.cardanofoundation.explorer.api.service.impl.MarketDataServiceImpl;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ValueOperations;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;

@ExtendWith(MockitoExtension.class)
public class MarketDataServiceTest {
  @Mock
  private WebClient webClient;
  @Mock
  private WebClient.RequestHeadersSpec requestHeadersMock;
  @Mock
  private WebClient.RequestHeadersUriSpec requestHeadersUriMock;
  @Mock
  private CustomMinimalForTestResponseSpec responseMock;

  @InjectMocks private MarketDataServiceImpl marketDataService;
  @Mock RedisTemplate<String, Object> redisTemplate;
  @Mock ValueOperations<String, Object> valueOperations;

  ObjectMapper objectMapper = new ObjectMapper();
  @BeforeEach
  void setUp() {
    ReflectionTestUtils.setField(marketDataService, "objectMapper", objectMapper);
    ReflectionTestUtils.setField(marketDataService, "apiMarketDataUrl", "localhost:8080");
  }

  @Test
  void testGetMarketData_thenReturn() throws JsonProcessingException {
    String currency = "usd";
    when(redisTemplate.opsForValue()).thenReturn(valueOperations);
    when(valueOperations.get(any())).thenReturn(null);
    ReflectionTestUtils.setField(marketDataService, "apiMarketDataUrl", "localhost:8080");
    when(webClient.get()).thenReturn(requestHeadersUriMock);
    when(requestHeadersUriMock.uri(String.format("localhost:8080", currency))).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Object.class)).thenReturn(Mono.just(prepareMarketData()));
    var response = marketDataService.getMarketData(currency);
    JsonNode jsonNode = objectMapper.valueToTree(response);
    Assertions.assertEquals(jsonNode.get(0).get("current_price").asText(), "0.250666");
  }

  Object prepareMarketData() throws JsonProcessingException {
    String marketDataString = "[\n"
        + "        {\n"
        + "            \"id\": \"cardano\",\n"
        + "            \"symbol\": \"ada\",\n"
        + "            \"name\": \"Cardano\",\n"
        + "            \"image\": \"https://assets.coingecko.com/coins/images/975/large/cardano.png?1547034860\",\n"
        + "            \"current_price\": 0.250666,\n"
        + "            \"market_cap\": 8781395398,\n"
        + "            \"market_cap_rank\": 8,\n"
        + "            \"fully_diluted_valuation\": 11275861265,\n"
        + "            \"total_volume\": 155156414,\n"
        + "            \"high_24h\": 0.250756,\n"
        + "            \"low_24h\": 0.243964,\n"
        + "            \"price_change_24h\": 0.00670147,\n"
        + "            \"price_change_percentage_24h\": 2.74691,\n"
        + "            \"market_cap_change_24h\": 230631606,\n"
        + "            \"market_cap_change_percentage_24h\": 2.6972,\n"
        + "            \"circulating_supply\": 3.50450208303234E10,\n"
        + "            \"total_supply\": 4.5E10,\n"
        + "            \"max_supply\": 4.5E10,\n"
        + "            \"ath\": 3.09,\n"
        + "            \"ath_change_percentage\": -91.88267,\n"
        + "            \"ath_date\": \"2021-09-02T06:00:10.474Z\",\n"
        + "            \"atl\": 0.01925275,\n"
        + "            \"atl_change_percentage\": 1201.501,\n"
        + "            \"atl_date\": \"2020-03-13T02:22:55.044Z\",\n"
        + "            \"last_updated\": \"2023-09-29T06:46:07.531046793\"\n"
        + "        }\n"
        + "    ]";
    return objectMapper.readValue(marketDataString, Object.class);
  }

  abstract class CustomMinimalForTestResponseSpec implements WebClient.ResponseSpec {
    public abstract HttpStatusCode getStatusCode();

    public WebClient.ResponseSpec onStatus(Predicate<HttpStatusCode> statusPredicate, Function<ClientResponse, Mono<? extends Throwable>> exceptionFunction) {
      if (statusPredicate.test(this.getStatusCode())) exceptionFunction.apply(ClientResponse.create(HttpStatus.OK).build()).block();
      return this;
    }
  }
}
