package org.cardanofoundation.explorer.api.model.response.drep;

import java.math.BigInteger;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DRepDelegatorsResponse {

  String stakeAddress;

  Date createdAt;

  Integer fee;

  BigInteger totalStake;
}
