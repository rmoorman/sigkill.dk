TeX in the Cloud
================

Just paste your TeX below and you'll receive a link to the resulting
PDF file.  Note that shell escapement is disabled for security
reasons, so a few packages may not work.  Otherwise, the entire set
from the `texlive-full` package in Debian is available.

And yes, you can include `/etc/passwd` if you really care.

%{
texcloud_root=$sitedir^$req_path
workdir=$texcloud_root/_werc/work/
infile=output.tex
outfile=output.pdf
logfile=output.log

fn compiletex {
   @{cd $"workdir;
     pdflatex -no-shell-escape -halt-on-error '-output-format=pdf' $"infile
    }
}

if (~ $REQUEST_METHOD POST) {
    echo $post_arg_tex_code > $"workdir/$"infile
    if (compiletex > /dev/null) {
        echo '<h3>Success</h3>'
        echo '<a href="./_werc/work/'^$outfile^'">Here is your file</a>'
    }
    if not {
        echo '<h3>TeX error</h3>'
        echo '<pre>'
        cat $"workdir/$"logfile
        echo '</pre>'
    }
}

%}

<form method="POST"><fieldset>
    <legend>Process this</legend>
    <textarea cols="80" rows=24" name="tex_code"></textarea><br />
    <input type="submit" value="Compile" />
</fieldset></form>
