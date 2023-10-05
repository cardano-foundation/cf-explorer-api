package org.cardanofoundation.explorer.api.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cardanofoundation.explorer.api.interceptor.auth.UserPrincipal;
import org.cardanofoundation.explorer.api.model.request.stake.report.ReportHistoryFilterRequest;
import org.cardanofoundation.explorer.api.projection.ReportHistoryProjection;
import org.cardanofoundation.explorer.api.repository.ReportHistoryRepository;
import org.cardanofoundation.explorer.api.service.impl.ReportHistoryServiceImpl;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportStatus;
import org.cardanofoundation.explorer.consumercommon.enumeration.ReportType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.junit.jupiter.api.Assertions;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
public class ReportHistoryServiceTest {

  @Mock
  ReportHistoryRepository reportHistoryRepository;

  @InjectMocks
  private ReportHistoryServiceImpl reportHistoryService;

  @Mock
  RoleService roleService;

  @Test
  void whenGetReportHistoryWithCondition_thenReturnReportHistory() {
    Pageable pageable = PageRequest.of(0, 1, Sort.by("createdAt").descending());
    Date fromDate = new Date();
    Date toDate = new Date();
    LocalDateTime createdAt = LocalDateTime.now();
    ReportHistoryFilterRequest condition = ReportHistoryFilterRequest.builder()
        .reportName("reportName")
        .fromDate(fromDate)
        .toDate(toDate)
        .build();

      ReportHistoryProjection projection = Mockito.mock(ReportHistoryProjection.class);
    when(projection.getReportName()).thenReturn("reportName");
    when(projection.getCreatedAt()).thenReturn(createdAt);
    when(projection.getStakeKeyReportId()).thenReturn(1L);
    when(projection.getType()).thenReturn(ReportType.STAKE_KEY);
    when(projection.getStatus()).thenReturn(ReportStatus.GENERATED);
    Page<ReportHistoryProjection> page = new PageImpl<>(List.of(projection), pageable, 1);

    when(reportHistoryRepository.getRecordHistoryByFilter(anyString(), any(Timestamp.class),
        any(Timestamp.class), anyString(), any(Pageable.class)))
        .thenReturn(page);

    var response = reportHistoryService.getReportHistory(condition, "username", pageable);
    Assertions.assertEquals(1, response.getTotalPages());
    Assertions.assertEquals(1, response.getTotalItems());
    Assertions.assertEquals(0, response.getCurrentPage());
    Assertions.assertEquals(1, response.getData().size());
    Assertions.assertEquals("reportName", response.getData().get(0).getReportName());
    Assertions.assertEquals(ReportType.STAKE_KEY, response.getData().get(0).getType());
    Assertions.assertEquals(ReportStatus.GENERATED, response.getData().get(0).getStatus());
    Assertions.assertEquals(1L, response.getData().get(0).getStakeKeyReportId());
    Assertions.assertEquals(createdAt, response.getData().get(0).getCreatedAt());
  }

  @Test
  void isLimitReached_shouldReturnTrueIfLimitIsReached() {
    when(reportHistoryRepository.countByUsernameAndCreatedAtBetween(anyString(),
                                                                    any(Timestamp.class),
                                                                    any(Timestamp.class)))
        .thenReturn(5);
    Assertions.assertTrue(reportHistoryService.isLimitReached("username",5));
  }

  @Test
  void isLimitReached_shouldReturnFalseIfLimitIsNotReached() {
    when(reportHistoryRepository.countByUsernameAndCreatedAtBetween(anyString(),
                                                                    any(Timestamp.class),
                                                                    any(Timestamp.class)))
        .thenReturn(4);
    Assertions.assertFalse(reportHistoryService.isLimitReached("username",5));
  }

  @Test
  void getReportLimitPer24Hours_shouldReturnLimit() {
    Map<String, Map<String,Object>> roleDescriptions = new HashMap<>();
    when(reportHistoryRepository.countByUsernameAndCreatedAtBetween(anyString(),
                                                                    any(Timestamp.class),
                                                                    any(Timestamp.class)))
        .thenReturn(4);
    when(roleService.getReportLimit(roleDescriptions)).thenReturn(5);
    var reportLimit = reportHistoryService.getReportLimit(UserPrincipal.builder().username("username").roleDescription(roleDescriptions).build());
    Assertions.assertEquals(5, reportLimit.getLimitPer24hours());
    Assertions.assertEquals(Boolean.FALSE, reportLimit.getIsLimitReached());
  }
}
