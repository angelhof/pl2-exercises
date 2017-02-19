<?php
if ("${_SERVER['QUERY_STRING']}" == "")
  $self = "${_SERVER['PHP_SELF']}";
else
  $self = "${_SERVER['PHP_SELF']}?${_SERVER['QUERY_STRING']}";


session_start();

function generate() {

  $num = rand(100000000,
              1000000000);
  $_SESSION['number'] = $num;
}

function evaluate_program($program) {
 
  // TODO: Make the API
  $api_url = "http://courses.softlab.ntua.gr/pl2/2016b/exercises/befunge93-api/?restrict";
  $api_url = "http://localhost:5000/befunge-api/?restrict";
  $data = trim($program, "\r\n") . "\r\n";

  $ch = curl_init();
  curl_setopt($ch, CURLOPT_URL, $api_url);
  curl_setopt($ch, CURLOPT_POST, true);
  curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($ch, CURLOPT_HTTPHEADER, array("Content-Type: text/plain")); 
  curl_setopt($ch, CURLOPT_POSTFIELDS, $data);
  $output = curl_exec($ch);
  $response = curl_getinfo($ch, CURLINFO_HTTP_CODE);
  curl_close($ch);
  
  //Debug
  //printf("<p class=\"debug\">$output</p>");
  //printf("<p class=\"debug\">$response</p>");

  $_SESSION['api-response'] = $response;
  return $output;
}

function compute_program_area($program){
  $array = preg_split("/\r\n|\n|\r/", $program);
  $a = count($array);
  $b = 0;
  for ($i = 0; $i < $a; $i++){
    if (strlen($array[$i]) > $b) {
      $b = strlen($array[$i]);
    }
  }
  $area = $a * $b;
  $result = "$a". " x ". "$b". " = ". "$area"; 
  return $result;
}

?>





<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Funge game!</title>
<style type="text/css">
<!--
body,td,th {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: x-large;
  color: #CCCCCC;
}

body {
  background-color: #333399;
}

.title {
  font-family: "Courier New", Courier, monospace;
  font-weight: bold;
  font-size: 48px;
  color: #00FF66;
}

.question {color: #FFCC33}
.number {color: #FFFF33}
.md5sum {color: #FFCCFF}
.emph {color: #99ee99}
.alert {color: #ee77aa}

.right {
  color: #33FF66;
  font-weight: bold;
}

.wrong {
  color: #FF3366;
  font-weight: bold;
}

a:link {
  color: #CCFFFF;
}

a:visited {
  color: #CCFFFF;
}

textarea {
  background-color: #eeee66;
  color: #333399;
}

textarea.wide {
  font-family: monospace;
  font-size: x-large;
  color: #333333;
  border: 1px solid black;
  padding: 8px;
}
-->
</style>
</head>
<body>

<?php
if (isset($_SESSION['generate']) || !isset($_SESSION['number'])) {
  generate();
}
unset($_SESSION['generate']);
?>

<h1 class="title">Help!</h1>
<p>I need to find a <a href="http://catseye.tc/view/befunge-93/doc/Befunge-93.markdown">Befunge-93</a> program that outputs the number <span class="question"><?php echo "${_SESSION['number']}"; ?></span>.</p>
<p>But I also need the program's total area to be as small as possible.<br>
(Don't worry, it doesn't have to be optimal...)</p>
<p>Oh, one more thing: The commands
  <code class="emph">0-9</code>,
  <code class="emph">?</code>,
  <code class="emph">"</code>,
  <code class="emph">p</code>,
  <code class="emph">g</code>,
  <code class="emph">&amp;</code>, and
  <code class="emph">~</code>
  cannot be used.</p>
  
<p>Enter your program that will print this number!</p>
<form <?php echo "action=\"$self\""; ?> id="f" name="f" method="post">
  <?php
      
      if (isset($_POST['submit'])) {
        // TODO: Logic if there exists and answer
        $result = evaluate_program($_POST['program']);
        if($_SESSION['api-response'] == 200){
          if ($result == $_SESSION['number']) {
            printf("<p class=\"right\">CORRECT :) </p> <br>\n");
            $program_area = compute_program_area($_POST['program']);
            printf("<p>Program area: $program_area.</p> <br>\n");
          }
          else {
            printf("<p class=\"wrong\">WRONG</p>\n");
          }
        }
        else{
          printf("<p class=\"wrong\">WRONG</p>\n");
          printf("<p class=\"info\">$result</p>\n");
        }
        $_SESSION['generate'] = true;
        printf("<input type=\"submit\" name=\"again\"
                           id=\"again\" value=\"Play again!\" />\n");
        
        
      }
      else {
        
        if (isset($_POST['reset'])) {
          printf("<span class=\"wrong\">You are a coward :'( Changing 
                    the number won't help you to beat the game</span>
                     <br> \n");
          $_SESSION['generate'] = true;
          printf("<input type=\"submit\" name=\"again\"
                   id=\"again\" value=\"Play again!\" />\n");
        }
        else{
          printf("<textarea name=\"program\" id=\"program\" 
                  class=\"wide\" rows=\"10\" cols=\"80\"></textarea>\n
                   <br> \n");
          printf("<input type=\"submit\" name=\"submit\"
                           id=\"submit\" value=\"Submit!\" />\n");
          printf("<input type=\"submit\" name=\"reset\"
                           id=\"reset\" value=\"Change number!\" />\n");  
        }
      }
?>
</form>


</body></html>