
def COLOR_MAP = [
    'SUCCESS': 'good',
    'FAILURE': 'danger',
]

def secretFolder = '~/configs/cardano-explorer-api'

pipeline {
    agent any

    stages {
        stage('Build') {
            steps {
                script{
                    sh "cp ${secretFolder}/settings.xml ./.m2/settings.xml"
                    sh "docker build -t cardano-explorer-api ."
                    echo 'Build successfully!!!'
                }
            }
        }

        stage('Deploy') {
            when {
                branch 'develop'
            }

            steps {
                echo 'Deploying....'
                script {
                    def envMainnet = secretFolder + "/mainnet.env"
                    def envPreprod = secretFolder + "/preprod.env"
                    sh "docker compose --env-file ${envMainnet}  -p mainnet up -d --build"
                    sh "docker compose --env-file ${envPreprod}  -p preprod up -d --build"

                }
 
                sh "docker images -f 'dangling=true' -q --no-trunc | xargs --no-run-if-empty docker rmi"
                echo 'Deployment Done!!!'
            }
        }
    }
    post {
        always {
            script {
                Author_ID = sh(script: "git show -s --pretty=%an", returnStdout: true).trim()
                Author_Name = sh(script: "git show -s --pretty=%ae", returnStdout: true).trim()
            }
            slackSend channel: "#build-group-test",
                    color: COLOR_MAP[currentBuild.currentResult],
                    message: "*${currentBuild.currentResult}:* ${env.JOB_NAME} build ${env.BUILD_NUMBER} by commit author: ${Author_ID} \n More information at: ${env.BUILD_URL}"
        }
    }
}
