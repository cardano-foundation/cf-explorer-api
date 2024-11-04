package org.cardanofoundation.explorer.api.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.cardanofoundation.cfexploreraggregator.AddressTxCountRecord;
import org.cardanofoundation.explorer.api.service.ExplorerAggregatorService;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Log4j2
public class ExplorerAggregatorServiceImpl implements ExplorerAggregatorService {

    @Value("${application.api.explorer-aggregator.base-url}")
    private String explorerAggregatorBaseUrl;


    @Override
    public Optional<AddressTxCountRecord> getTxCountForAddress(String address) {
        RestTemplate restTemplate = new RestTemplate();
        ResponseEntity<AddressTxCountRecord> forEntity = restTemplate.getForEntity(explorerAggregatorBaseUrl + "/addresstxcount/" + address, AddressTxCountRecord.class);
        if (forEntity.getStatusCode().is2xxSuccessful()) {
            return Optional.ofNullable(forEntity.getBody());
        }
        return Optional.empty();
    }

    @Override
    public List<AddressTxCountRecord> getAllTxCount(Pageable pageable) {
        return List.of();
    }
}
