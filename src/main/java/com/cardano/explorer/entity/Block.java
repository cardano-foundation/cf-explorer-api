package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Hash32Type;
import com.cardano.explorer.validation.Word31Type;
import com.cardano.explorer.validation.Word63Type;
import java.sql.Timestamp;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(name = "block", uniqueConstraints = {
    @UniqueConstraint(name = "unique_block", columnNames = {"hash"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class Block extends BaseEntity {

  @Column(name = "hash", nullable = false, unique = true)
  @Hash32Type
  private byte[] hash;

  @Column(name = "epoch_no")
  @Word31Type
  private Integer epochNo;

  @Column(name = "slot_no")
  @Word63Type
  private Long slotNo;

  @Column(name = "epoch_slot_no")
  @Word31Type
  private Integer epochSlotNo;

  @Column(name = "block_no", nullable = false)
  private Long blockNo;

  @ManyToOne(fetch = FetchType.LAZY)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "previous_id", foreignKey = @ForeignKey(name = "block_previous_id_fkey") )
  @EqualsAndHashCode.Exclude
  private Block previous;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "slot_leader_id", nullable = false,
      foreignKey = @ForeignKey(name = "block_slot_leader_id_fkey"))
  @EqualsAndHashCode.Exclude
  private SlotLeader slotLeader;

  @Column(name = "size", nullable = false)
  @Word31Type
  private Integer size;

  @Column(name = "time", nullable = false)
  private Timestamp time;

  @Column(name = "tx_count", nullable = false)
  private Long txCount;

  @Column(name = "proto_major", nullable = false)
  @Word31Type
  private Integer protoMajor;

  @Column(name = "proto_minor", nullable = false)
  @Word31Type
  private Integer protoMinor;

  @Column(name = "vrf_key")
  @Lob
  private String vrfKey;

  @Column(name = "op_cert")
  @Hash32Type
  private byte[] opCert;

  @Column(name = "op_cert_counter")
  @Word63Type
  private Long opCertCounter;

}
