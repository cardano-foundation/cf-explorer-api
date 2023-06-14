package tests.contract;

import base.BaseTest;
import io.qameta.allure.Epic;
import io.qameta.allure.Feature;
import microservices.contract.models.Contract;
import microservices.contract.models.DataContract;
import microservices.contract.steps.ContractSteps;

import org.apache.commons.collections.MultiMap;
import org.apache.commons.collections.map.MultiValueMap;

import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;
import util.CreateParameters;

import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.Map;

@Epic("cardano")
@Feature("api-contract")
public class ContractTests extends BaseTest {
    private ContractSteps contractSteps = new ContractSteps();
    private Contract contract = new Contract();
    private ArrayList<DataContract> dataContracts;

    @Test(description = "verify that get list contract successfully", groups={"contract"},dataProvider = "paramSuccess")
    public void getListParamSuccess(String page, String size, String sort){
        MultiMap params = new MultiValueMap();
        params.put("page", page);
        params.put("size", size);
        if(!sort.equals("")){
            params.put("sort", sort);
        }
        contract = (Contract) contractSteps.getListContracts(params)
                .validateResponse(HttpURLConnection.HTTP_OK)
                .saveResponseObject(Contract.class);

        dataContracts = contract.getData();
        contractSteps.then_verifyContractResponse(contract,params)
                     .verifyResponseDataNotNull(dataContracts);
    }
    @DataProvider(name="paramSuccess")
    public Object[][] dataSetSuccess(){
        return new Object[][]{
                {"2","2",""},
                {"1","",""},
                {"","2",""},
                {"","",""},
        };
    }

    @Test(description = "verify that get list contract with invalid data", groups={"contract"},dataProvider = "paramUnSuccess")
    public void getListParamUnSuccess(String page, String size, String sort){
        Map<String, Object> param = new CreateParameters()
                .withPage(page)
                .withPageSize(size)
                .withSort(sort)
                .getParamsMap();
        contractSteps.getListContracts(param)
                .validateResponse(HttpURLConnection.HTTP_OK);
    }
    @DataProvider(name="paramUnSuccess")
    public Object[][] dataSetUnSuccess(){
        return new Object[][]{
                {"12222222","",""},
                {"12222222222222222222","",""},
                {"@#$","",""},
                {"","123",""},
                {"","abc",""},
        };
    }
}
