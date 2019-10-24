# StringVar
Written by Janez Atmapuri Makovsek, 4th. June 2008, www.dewresearch.com

Stringlist is a replacement for TStringList for Delphi 2006 and after. 

## Usage Example:
```delphi
    procedure TScriptingForm.Button1Click(Sender: TObject);
    var strings: StringList;
        astr: string;
    begin
        strings.Add('test1');
        strings.Add('test2');
        Caption := string(strings);
        RichEdit.Lines.AddStrings(strings);
    end;
```

It is basically the same as TStringList except that it is a value class. It does not have to be created, destroyed or put within try/finally. This is done by the compiler for you. There are virtually no special performance penalties for these to work.

You can use the same template to make value classes out of your other objects also. This code is free to use. 
