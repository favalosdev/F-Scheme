import subprocess
import os
import logging

WORKING_DIRECTORY = '/home/lambdaronin/Documents/projects/F-Scheme'

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

def run_test(test_id):
    test_directory = os.path.join('test', 'cases', test_id)
    test_file = os.path.join(test_directory, 'test.scm')
    
    logger.info(f"Running test suite: {test_id}")
    logger.debug(f"Test file: {test_file}")

    output = subprocess.run(['cabal', 'run', 'f-scheme', test_file], cwd=WORKING_DIRECTORY, capture_output=True)
    s = output.stderr.decode('utf-8')
    guess = s[s.find('(')+1:s.find(')')].split()

    answer_file = os.path.join(WORKING_DIRECTORY, test_directory, 'answer.txt')
    
    with open(answer_file, 'r') as f:
        answer = f.read().splitlines()
        G = len(guess)
        A = len(answer)

        if G != A:
            logger.error(f"Dimension mismatch: expected {A} answers, got {G}")
            return

        success_count = 0
        for i in range(G):
            if guess[i] == answer[i]:
                logger.info(f"Test {i+1}: PASS")
                success_count += 1
            else:
                logger.error(f"Test {i+1}: FAIL (expected '{answer[i]}', got '{guess[i]}')")
        
        success_rate = (success_count / G) * 100
        logger.info(f"Test Results: {success_count}/{G} ({success_rate:.1f}%) tests passed")

if __name__ == '__main__':
    run_test(test_id='example')
