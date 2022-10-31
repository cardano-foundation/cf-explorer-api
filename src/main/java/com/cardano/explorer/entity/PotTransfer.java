package com.cardano.explorer.entity;

import com.cardano.explorer.validation.Int65Type;
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
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

@Entity
@Table(name = "pot_transfer", uniqueConstraints = {
    @UniqueConstraint(name = "unique_pot_transfer",
        columnNames = {"tx_id", "cert_index"})
})
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(onlyExplicitlyIncluded = true, callSuper = false)
@SuperBuilder(toBuilder = true)
public class PotTransfer extends BaseEntity {

  @Column(name = "cert_index", nullable = false)
  private Integer certIndex;

  @Column(name = "treasury", nullable = false)
  @Int65Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal treasury;

  @Column(name = "reserves", nullable = false)
  @Int65Type
  @Digits(integer = 20, fraction = 0)
  private BigDecimal reserves;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @OnDelete(action = OnDeleteAction.CASCADE)
  @JoinColumn(name = "tx_id", nullable = false,
      foreignKey = @ForeignKey(name = "pot_transfer_tx_id_fkey"))
  @EqualsAndHashCode.Exclude
  private Tx tx;

}
