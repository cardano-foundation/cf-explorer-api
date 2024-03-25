package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.common.entity.ledgersync.enumeration.Vote;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class HistoryVote {
  Long no;

  Vote vote;

  Date timestamp;
}
