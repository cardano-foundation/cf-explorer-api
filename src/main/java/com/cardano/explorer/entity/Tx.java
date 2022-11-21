package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Hash32Type;
import com.cardano.explorer.validation.Lovelace;
import com.cardano.explorer.validation.Word31Type;
import com.cardano.explorer.validation.Word64Type;
import java.math.BigDecimal;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.Digits;
import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(name = "tx", uniqueConstraints = {
    @UniqueConstraint(name = "unique_tx",
        columnNames = {"hash"}
    )
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
@org.hibernate.annotations.Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Tx extends BaseEntity {

  @Column(name = "hash", nullable = false)
  @Hash32Type
  private byte[] hash;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "block_id", nullable = false,
      foreignKey = @ForeignKey(name = "tx_block_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Block block;

  @Column(name = "block_id", updatable = false, insertable = false)
  private Long blockId;

  @Column(name = "block_index", nullable = false)
  @Word31Type
  private Long blockIndex;

  @Column(name = "out_sum", nullable = false)
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal outSum;

  @Column(name = "fee", nullable = false)
  @Lovelace
  @Digits(integer = 20, fraction = 0)
  private BigDecimal fee;

  @Column(name = "deposit", nullable = false)
  private Long deposit;

  @Column(name = "size", nullable = false)
  @Word31Type
  private Integer size;

  @Column(name = "invalid_before")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal invalidBefore;

  @Column(name = "invalid_hereafter")
  @Word64Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal invalidHereafter;

  @Column(name = "valid_contract", nullable = false)
  private Boolean validContract;

  @Column(name = "script_size", nullable = false)
  @Word31Type
  private Integer scriptSize;

}
