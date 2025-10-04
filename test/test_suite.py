import subprocess
import os
import sys
from dotenv import load_dotenv

load_dotenv()

WORKING_DIRECTORY = "./"

def run_test(test_id):
    test_directory = os.path.join('test', 'cases', test_id)
    test_file = os.path.join(test_directory, 'test.scm')
    
    print(f"\nRunning test suite: {test_id}")
    print("=" * 40)

    output = subprocess.run(['cabal', 'run', 'f-scheme', test_file], cwd=WORKING_DIRECTORY, capture_output=True)
    s = output.stderr.decode('utf-8')
    guess = s[s.find('(')+1:s.find(')')].split()

    answer_file = os.path.join(WORKING_DIRECTORY, test_directory, 'answer.txt')
    
    with open(answer_file, 'r') as f:
        answer = f.read().splitlines()
        G = len(guess)
        A = len(answer)

        if G != A:
            print(f"❌ Dimension mismatch: expected {A} answers, got {G}")
            return

        success_count = 0
        for i in range(G):
            if guess[i] == answer[i]:
                print(f"✓ Test {i+1}: PASS")
                success_count += 1
            else:
                print(f"✗ Test {i+1}: FAIL")
                print(f"  Expected: '{answer[i]}'")
                print(f"  Got:      '{guess[i]}'")
        
        success_rate = (success_count / G) * 100
        print("\nSummary:")
        print("=" * 40)
        print(f"Tests passed: {success_count}/{G} ({success_rate:.1f}%)")
        if success_count == G:
            print("🎉 All tests passed!")
        elif success_count == 0:
            print("❌ All tests failed!")
        else:
            print("⚠️  Some tests failed")

def main():
    if len(sys.argv) != 2:
        print("Usage: python test_suite.py <test_category>")
        print("Available test categories:")
        print("  - core")
        print("  - parser")
        print("  - primitive")
        print("  - environment")
        print("  - error")
        # print("  - example")
        sys.exit(1)
    
    test_category = sys.argv[1]
    valid_categories = [
        'core',
        'parser',
        'primitive',
        'environment',
        'error',
        # 'example'
    ]
    
    if test_category not in valid_categories:
        print(f"Error: Invalid test category '{test_category}'")
        print("Valid categories are:", ', '.join(valid_categories))
        sys.exit(1)
    
    run_test(test_category)

if __name__ == '__main__':
    main()
