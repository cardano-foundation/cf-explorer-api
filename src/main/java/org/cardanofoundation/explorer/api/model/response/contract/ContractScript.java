package org.cardanofoundation.explorer.api.model.response.contract;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class ContractScript {
    private Boolean isVerified;
    private String data;
}
