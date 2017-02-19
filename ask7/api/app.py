#!flask/bin/python
from flask import Flask, request
from subprocess import call, Popen, PIPE
import threading

## From stackoverflow question: 
## http://stackoverflow.com/questions/1191374/using-module-subprocess-with-timeout
class Command(object):
    def __init__(self, cmd, input):
        self.cmd = cmd
        self.input = input
        self.process = None
        self.output = None
        self.return_code = None

    def run(self, timeout):
        def target():
            self.process = Popen(self.cmd, stdin=PIPE, stdout=PIPE)
            try:
                self.output = self.process.communicate(input=self.input)[0] 
                self.return_code = self.process.returncode
            except Exception as exn:
                self.process.terminate()
                return ("unavailable", "", 404)

        thread = threading.Thread(target=target)
        thread.start()

        thread.join(timeout)
        if thread.is_alive():
            self.process.terminate()
            thread.join()
            return ("timeout", self.output, self.return_code)

        return ("ok", self.output, self.return_code)

# Global Constants
befunge_interpreter = "/home/konstantinos/Desktop/University/9th_Semester/pl2/ask6/interpreter"

disallowed_chars = [str(x) for x in xrange(10)] + \
                   [ '?'
                   , '"'
                   , 'p'
                   , 'g'
                   , '&'
                   , '~']

app = Flask(__name__)

@app.route('/befunge-api/', methods=['POST'])
def post_program():
    program = request.get_data()
    #print program

    # Check whether program contains illegal commands
    query_string = request.query_string
    if("restrict" in query_string):
        for c in program:
            if c in disallowed_chars:
                return "Program contains illegal commands", 400


    # Execute the interpreter with a timeout
    command = Command([befunge_interpreter, 'a', '-stdin'], program)
    (status, output, return_code) = command.run(timeout=1)

    # Handle return status
    if(status == "unavailable"):
        return "Server Unavailable", 404
    elif(status == "timeout"):
        return "Timeout", 401

    # Handle an execution error
    if(return_code != 0):
        return "Befunge Execution Error!" + output, 400    
    
    #print output
    return output, 200


if __name__ == '__main__':
    app.run(debug=True)