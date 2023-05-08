package org.cardanofoundation.explorer.api.service.impl;

import org.cardanofoundation.explorer.api.common.enumeration.EpochStatus;
import org.cardanofoundation.explorer.api.exception.BusinessCode;
import org.cardanofoundation.explorer.api.mapper.EpochMapper;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.EpochResponse;
import org.cardanofoundation.explorer.api.model.response.dashboard.EpochSummary;
import org.cardanofoundation.explorer.api.repository.EpochRepository;
import org.cardanofoundation.explorer.api.service.EpochService;
import org.cardanofoundation.explorer.common.exceptions.BusinessException;
import org.cardanofoundation.explorer.consumercommon.entity.Epoch;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Random;
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
      var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
          () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
      checkEpochStatus(response, currentEpoch);
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
    var currentEpoch = epochRepository.findCurrentEpochNo().orElseThrow(
        () -> new BusinessException(BusinessCode.EPOCH_NOT_FOUND));
    pageResponse.getContent().forEach(epoch -> checkEpochStatus(epoch, currentEpoch));
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
  private void checkEpochStatus(EpochResponse epoch, Integer currentEpoch) {
    var rewardTime = LocalDateTime.now().minusDays(10);
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
              //.account(epochRepository.getTotalAccountsAtEpoch(epochSummaryProjection.getNo()))
              .account(new Random().nextInt())
              .build();
        })
        .orElse(EpochSummary.builder().slot(0).no(0).totalSlot(0).build());
  }

}
