import sys
import random
import os

RANDOM_SEED = 0.8723737450417004
random.seed(RANDOM_SEED)

def generate_protocol(n, current_depth=0, msg_count=1, qos = True):
    protocol = ""

    # base case for the recursion
    if n == 0:
        # add message number to the leaf node
        sender = "Alice" if n % 2 == 1 else "Bob"
        receiver = "Bob" if n % 2 == 1 else "Alice"
        indentation = " " * (current_depth * 4)
        protocol += f"{indentation}{sender} -> {receiver}: leaf{msg_count}"
        if qos:
            protocol +=  f"{{ sqos: (= x1 {msg_count}) (= x2 {msg_count}) (= x3 {msg_count}) (= x4 {msg_count}) (= x5 {msg_count}) }}"
        protocol += "\n"
        return protocol

    sender = "Alice" if n % 2 == 1 else "Bob"
    receiver = "Bob" if n % 2 == 1 else "Alice"

    indentation = " " * (current_depth * 4)

    protocol += f"{indentation}sel {sender} {{\n"
    protocol += f"{indentation}    {sender} -> {receiver}: m0\n"
    protocol += f"{indentation}    ;\n" + generate_protocol(n-1, current_depth+1, msg_count, qos=qos)
    protocol += f"{indentation}    +\n"
    # increment message count for the right branch
    protocol += f"{indentation}    {sender} -> {receiver}: m1\n"
    protocol += f"{indentation}    ;\n" + generate_protocol(n-1, current_depth+1, msg_count+2**(n-1), qos=qos)
    protocol += f"{indentation}}}\n"

    return protocol

def generate_formula(height):
    output = "True U [\n"
    output += generate_protocol(height, qos=False)
    output += "]\n"

    random_n = random.randint(1, 2**(height))
    output += f"qos{{(= x1 {random_n}) (= x2 {random_n}) (= x3 {random_n}) (= x4 {random_n}) (= x5 {random_n})}}"

    return output

if __name__ == "__main__":
    
    if len(sys.argv) != 2:
        print("Usage: python generate_data.py <mode>")
        print("mode: 'prop' or 'sys'")
        exit(1)

    mode = sys.argv[1]
    current_directory = os.path.dirname(os.path.realpath(__file__))
    
    if mode == "prop":
        prop_base_directory = os.path.join(current_directory, "prop")
        for height in range(1, 11):
            prop_directory = os.path.join(prop_base_directory, str(height))
            os.makedirs(prop_directory, exist_ok=True)
            for i in range(1, 101):
                output = generate_formula(height)
                with open(os.path.join(prop_directory, f"prop-{i}.ql"), "w") as f:
                    f.write(output)
    elif mode == "sys":
        sys_base_directory = os.path.join(current_directory, "sys")
        os.makedirs(sys_base_directory, exist_ok=True)
        for height in range(1, 11):
            output = generate_protocol(height)
            output += "; accept\n"
            output += "\n qos {x1:+,x2:+,x3:+,x4:+,x5:+}"
            with open(os.path.join(sys_base_directory, f"sys-{height}.qosgc"), "w") as f:
                f.write(output)

        '''
        for i in {1..10}; do cabal run -v0 project -- -D min -qos experiments/mocheqos/performance/nested_choices/sys/sys_$i.qosgc > experiments/mocheqos/performance/nested_choices/sys/sys_$i.qosfsa; done
        '''
            
    else:
        raise ValueError("Mode must be either 'prop' or 'sys'")