package com.cardano.explorer.entity;

import com.cardano.explorer.common.enumeration.SyncStateType;
import com.cardano.explorer.validation.Word63Type;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Entity
@Table(name = "epoch_sync_time", uniqueConstraints = {
    @UniqueConstraint(name = "unique_epoch_sync_time",
        columnNames = {"no"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class EpochSyncTime extends BaseEntity {

  @Column(name = "no", nullable = false)
  private Long no;

  @Column(name = "seconds", nullable = false)
  @Word63Type(message = "seconds must be word63type")
  private Long seconds;

  @Column(name = "state", nullable = false)
  @Enumerated(EnumType.STRING)
  private SyncStateType state;

}
