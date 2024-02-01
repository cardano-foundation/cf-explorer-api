package org.cardanofoundation.explorer.api.service;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;

import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.WebClient;

import org.cardanofoundation.explorer.api.repository.ledgersync.AdaPotsRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.EpochStakeCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.PoolHistoryCheckpointRepository;
import org.cardanofoundation.explorer.api.repository.ledgersync.RewardCheckpointRepository;
import org.cardanofoundation.explorer.api.service.impl.FetchRewardDataFromKoiosServiceImpl;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import static org.mockito.ArgumentMatchers.anyString;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(MockitoExtension.class)
public class FetchRewardDataFromKoiosServiceTest {
  @InjectMocks
  private FetchRewardDataFromKoiosServiceImpl fetchRewardDataFromKoiosService;
  @Mock
  private RewardCheckpointRepository rewardCheckpointRepository;
  @Mock
  private PoolHistoryCheckpointRepository poolHistoryCheckpointRepository;
  @Mock
  private EpochStakeCheckpointRepository epochStakeCheckpointRepository;
  @Mock
  private AdaPotsRepository adaPotsRepository;
  @Mock
  private WebClient.RequestHeadersSpec requestHeadersMock;
  @Mock
  private WebClient.RequestHeadersUriSpec requestHeadersUriMock;
  @Mock
  private WebClient.RequestBodySpec requestBodyMock;
  @Mock
  private WebClient.RequestBodyUriSpec requestBodyUriMock;
  @Mock
  private CustomMinimalForTestResponseSpec responseMock;
  @Mock
  private WebClient webClient;
  @BeforeEach
  void setUp() {
    ReflectionTestUtils.setField(fetchRewardDataFromKoiosService, "apiCheckRewardUrl", "localhost:8080");
    ReflectionTestUtils.setField(fetchRewardDataFromKoiosService,"apiCheckPoolHistoryUrl","localhost:8080");
    ReflectionTestUtils.setField(fetchRewardDataFromKoiosService,"apiCheckEpochStakeUrl","localhost:8080");
    ReflectionTestUtils.setField(fetchRewardDataFromKoiosService,"apiCheckAdaPotsUrl","localhost:8080");
    ReflectionTestUtils.setField(fetchRewardDataFromKoiosService,"apiCheckEpochUrl","localhost:8080");
  }
  @Test
  void testCheckRewardAvailable_thenReturnFalse(){
    when(rewardCheckpointRepository.checkRewardByStakeAddressAndEpoch(anyString())).thenReturn(false);

    var result = fetchRewardDataFromKoiosService.checkRewardAvailable(anyString());

    Assertions.assertEquals(false,result);
  }

  @Test
  void testFetchRewards(){
    String stakeKey = "stakeKey";
    when(webClient.post()).thenReturn(requestBodyUriMock);
    when(requestBodyUriMock.uri("localhost:8080")).thenReturn(requestBodyMock);
    when(requestBodyMock.bodyValue(any())).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Boolean.class)).thenReturn(Mono.just(true));

    var actual = fetchRewardDataFromKoiosService.fetchReward(stakeKey);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testCheckRewardAvailable_thenReturn(){
    String stakeKey = "stakeKey";
    List<String> list = List.of(stakeKey);
    Integer count = 1;

    when(rewardCheckpointRepository.checkRewardByRewardAccountsAndEpoch(list)).thenReturn(count);

    var actual = fetchRewardDataFromKoiosService.checkRewardAvailable(list);

    Assertions.assertEquals(actual,true);

  }

  @Test
  void testFetchRewards_thenReturn(){
    String stakeKey = "stakeKey";
    List<String> list = List.of(stakeKey);
    when(webClient.post()).thenReturn(requestBodyUriMock);
    when(requestBodyUriMock.uri("localhost:8080")).thenReturn(requestBodyMock);
    when(requestBodyMock.bodyValue(list)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Boolean.class)).thenReturn(Mono.just(true));

    var actual = fetchRewardDataFromKoiosService.fetchReward(list);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testCheckPoolHistoryForPool_thenReturn(){
    String poolId = "poodId";
    Set<String> set = Set.of(poolId);
    Integer size = 1;

    when(poolHistoryCheckpointRepository.checkRewardByPoolViewAndEpoch(set)).thenReturn(size);

    var actual = fetchRewardDataFromKoiosService.checkPoolHistoryForPool(set);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testFetchPoolHistoryForPool_thenReturn(){
    String poolId = "poodId";
    Set<String> set = Set.of(poolId);

    when(webClient.post()).thenReturn(requestBodyUriMock);
    when(requestBodyUriMock.uri("localhost:8080")).thenReturn(requestBodyMock);
    when(requestBodyMock.bodyValue(set)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Boolean.class)).thenReturn(Mono.just(true));

    var actual = fetchRewardDataFromKoiosService.fetchPoolHistoryForPool(set);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testCheckEpochStakeForPool(){
    List<String> rewardsAccount = List.of("1","2");
    Integer size = 2;

    when(epochStakeCheckpointRepository.checkEpochStakeByAccountsAndEpoch(rewardsAccount)).thenReturn(size);

    var actual = fetchRewardDataFromKoiosService.checkEpochStakeForPool(rewardsAccount);
    Assertions.assertEquals(actual,true);
  }

  @Test
  void testFetchEpochStakeForPool(){
    List<String> rewardAccounts = List.of("1","2");

    when(webClient.post()).thenReturn(requestBodyUriMock);
    when(requestBodyUriMock.uri("localhost:8080")).thenReturn(requestBodyMock);
    when(requestBodyMock.bodyValue(rewardAccounts)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Boolean.class)).thenReturn(Mono.just(true));

    var actual = fetchRewardDataFromKoiosService.fetchEpochStakeForPool(rewardAccounts);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testCheckRewardForPool(){
    List<String> rewardAccounts = List.of("1","2","3");

    when(rewardCheckpointRepository.checkRewardByRewardAccountsAndEpoch(rewardAccounts)).thenReturn(rewardAccounts.size());

    var actual = fetchRewardDataFromKoiosService.checkRewardForPool(rewardAccounts);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testFetchRewardForPool(){
    List<String> rewardAccounts = List.of("1","2","3");

    when(webClient.post()).thenReturn(requestBodyUriMock);
    when(requestBodyUriMock.uri("localhost:8080")).thenReturn(requestBodyMock);
    when(requestBodyMock.bodyValue(rewardAccounts)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Boolean.class)).thenReturn(Mono.just(true));

    var actual = fetchRewardDataFromKoiosService.fetchRewardForPool(rewardAccounts);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testCheckAdaPots(){
    Integer epochNo = 1;

    when(adaPotsRepository.existsByEpochNo(epochNo)).thenReturn(true);

    var actual = fetchRewardDataFromKoiosService.checkAdaPots(epochNo);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testFetchAdaPots(){
    List<Integer> epochNo = List.of(1,2);

    when(webClient.post()).thenReturn(requestBodyUriMock);
    when(requestBodyUriMock.uri("localhost:8080")).thenReturn(requestBodyMock);
    when(requestBodyMock.bodyValue(epochNo)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Boolean.class)).thenReturn(Mono.just(true));

    var actual = fetchRewardDataFromKoiosService.fetchAdaPots(epochNo);

    Assertions.assertEquals(actual,true);
  }

  @Test
  void testFetchEpochRewardDistributed(){
    List<Integer> epochNoList = List.of(1,2);
    Epoch epoch = Epoch.builder().no(1).build();
    Epoch[] epoches = new Epoch[]{epoch};

    when(webClient.post()).thenReturn(requestBodyUriMock);
    when(requestBodyUriMock.uri("localhost:8080")).thenReturn(requestBodyMock);
    when(requestBodyMock.bodyValue(epochNoList)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.acceptCharset(StandardCharsets.UTF_8)).thenReturn(requestHeadersMock);
    when(requestHeadersMock.retrieve()).thenReturn(responseMock);
    when(responseMock.getStatusCode()).thenReturn(HttpStatusCode.valueOf(200));
    when(responseMock.onStatus(any(Predicate.class),any(Function.class))).thenCallRealMethod();
    when(responseMock.bodyToMono(Epoch[].class)).thenReturn(Mono.just(epoches));

    var actual = fetchRewardDataFromKoiosService.fetchEpochRewardDistributed(epochNoList);

    Assertions.assertEquals(true,actual.contains(epoch));
  }

  @Test
  void testUseKoios(){
    var actual = fetchRewardDataFromKoiosService.useKoios();
    Assertions.assertEquals(actual,true);
  }

  abstract class CustomMinimalForTestResponseSpec implements WebClient.ResponseSpec {
    public abstract HttpStatusCode getStatusCode();

    public WebClient.ResponseSpec onStatus(Predicate<HttpStatusCode> statusPredicate, Function<ClientResponse, Mono<? extends Throwable>> exceptionFunction) {
      if (statusPredicate.test(this.getStatusCode())) exceptionFunction.apply(ClientResponse.create(
          HttpStatus.OK).build()).block();
      return this;
    }
  }
}
