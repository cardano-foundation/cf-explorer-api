package org.cardanofoundation.explorer.api.model.request.governanceAction;

import java.time.LocalDateTime;

import jakarta.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.springframework.format.annotation.DateTimeFormat;

import org.cardanofoundation.explorer.common.entity.enumeration.GovActionType;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class GovCommitteeHistoryFilter {
  String governanceActionTxHash;
  String anchorText;

  @NotNull GovActionType actionType;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime fromDate;

  @DateTimeFormat(pattern = "yyyy/MM/dd HH:mm:ss")
  private LocalDateTime toDate;
}
