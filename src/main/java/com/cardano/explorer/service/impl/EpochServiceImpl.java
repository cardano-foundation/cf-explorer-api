package com.cardano.explorer.service.impl;

import com.cardano.explorer.common.enumeration.EpochStatus;
import com.cardano.explorer.exception.BusinessCode;
import com.cardano.explorer.mapper.EpochMapper;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.EpochResponse;
import com.cardano.explorer.model.response.dashboard.EpochSummary;
import com.cardano.explorer.repository.EpochRepository;
import com.cardano.explorer.service.EpochService;
import com.sotatek.cardano.common.entity.Epoch;
import com.sotatek.cardanocommonapi.exceptions.BusinessException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class EpochServiceImpl implements EpochService {

  public static final int MILLI = 1000;
  private final EpochRepository epochRepository;
  private final EpochMapper epochMapper;

  @Override
  @Transactional(readOnly = true)
  public EpochResponse getEpochDetail(String no) {
    try {
      Integer epochNo = Integer.parseInt(no);
      Epoch epoch = epochRepository.findFirstByNo(epochNo).orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND)
      );
      EpochResponse response = epochMapper.epochToEpochResponse(epoch);
      checkEpochStatus(response);
      return response;
    } catch (NumberFormatException e) {
      throw new BusinessException(BusinessCode.EPOCH_NOT_FOUND);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public BaseFilterResponse<EpochResponse> getAllEpoch(Pageable pageable) {
    Page<Epoch> epochs = epochRepository.findAll(pageable);
    Page<EpochResponse> pageResponse = epochs.map(epochMapper::epochToEpochResponse);
    pageResponse.getContent().forEach(this::checkEpochStatus);
    return new BaseFilterResponse<>(pageResponse);
  }

  /**
   * Get epoch status from start time and end time
   *
   * <p>Start time < now < end time : in progress</p>
   * <p>End time > now - 10 day and not in progress: rewarding</p>
   * <p>Others: finished</p>
   *
   * @param epoch epoch response
   */
  private void checkEpochStatus(EpochResponse epoch) {
    var rewardTime = LocalDateTime.now().minusDays(10);
    var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
        () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
    if (epoch.getStartTime().plusDays(5).isAfter(LocalDateTime.now(ZoneId.of("UTC")))
     && epoch.getStartTime().isBefore(LocalDateTime.now(ZoneId.of("UTC")))) {
      epoch.setStatus(EpochStatus.IN_PROGRESS);
      epoch.setEndTime(epoch.getStartTime().plusDays(5));
    } else if (rewardTime.isBefore(epoch.getEndTime())) {
      epoch.setStatus(EpochStatus.REWARDING);
    } else {
      epoch.setStatus(EpochStatus.FINISHED);
    }
    if(!EpochStatus.IN_PROGRESS.equals(epoch.getStatus()) && currentEpoch.equals(epoch.getNo())) {
      epoch.setStatus(EpochStatus.SYNCING);
    }
  }

  @Override
  @Transactional(readOnly = true)
  public EpochSummary getCurrentEpochSummary() {

    return epochRepository
        .findCurrentEpochSummary()
        .map(epochSummaryProjection -> {
          var slot =
              (Instant.now().toEpochMilli() - epochSummaryProjection.getStartTime().getTime())
                  / MILLI;
          return EpochSummary.builder()
              .no(epochSummaryProjection.getNo())
              .slot((int) slot)
              .totalSlot(epochSummaryProjection.getMaxSlot())
              .build();
        })
        .orElse(EpochSummary.builder().slot(0).no(0).totalSlot(0).build());
  }

}
