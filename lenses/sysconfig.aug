(* Variation of the Shellvars lens                                     *)
(* Supports only what's needed to handle sysconfig files               *)
(* Modified to strip quotes. In the put direction, add double quotes   *)
(* around values that need them                                        *)
(* To keep things simple, we also do not support shell variable arrays *)
module Sysconfig =
  autoload xfm

  let eol = Util.eol

  let key_re = /[A-Za-z0-9_]+(\[[0-9]+\])?/ - "unset" - "export"
  let eq = Util.del_str "="

  let empty   = Util.empty_generic /[ \t]*#?[ \t]*/
  let yast = [ key /[A-Za-z]+/ . del /:[ \t]+/ ":\t" . store /([^ \t\n][^\n].*[^ \t\n]|[^ \t\n])/ . eol ]
  let option  = empty* . del /[ \t]*##[ \t]+/ "## " . yast
  let comment = empty* . [ label "#comment" . del /[ \t]*#[ \t]*/ "# " . store /([^ \t\n#].*[^ \t\n]|[^ \t\n])/ . eol ]
  let dels    = Util.del_str

  let nothing = del /(""|'')?/ "" . value ""

  (* Chars allowed in a bare string *)
  let bchar = /[^ \t\n\"'\\]|\\\\./
  let qchar = /["']/  (* " *)

  (* We split the handling of right hand sides into a few cases:
   *   bare  - strings that contain no spaces, optionally enclosed in
   *           single or double quotes
   *   dquot - strings that contain at least one space or apostrophe,
   *           which must be enclosed in double quotes
   *   squot - strings that contain an unescaped double quote
   *)
  let bare = del qchar? "" . store (bchar+) . del qchar? ""
  let dquot =
    del qchar "\"" . store (bchar* . /[ \t']/ . bchar*)+ . del qchar "\""
  let squot =
    dels "'" . store ((bchar|/[ \t]/)* . "\"" . (bchar|/[ \t]/)*)+ . dels "'"

  let export = [ key "export" . Util.del_ws_spc ]
  let kv (value:lens) = key key_re . eq . [ label "value" . value ] . eol
  let assign = empty* . ( kv nothing | kv bare | kv dquot | kv squot )

  let record = [ option* . comment* . assign ]

  let lns = record* . empty*

  let sc_incl (n:string) = (incl ("/etc/sysconfig/" . n))
  let filter_sysconfig =
      sc_incl "atd" .
      sc_incl "authconfig" .
      sc_incl "autofs" .
      sc_incl "bootloader" .
      sc_incl "clock" .
      sc_incl "cpuspeed" .
      sc_incl "crond" .
      sc_incl "crontab" .
      sc_incl "desktop" .
      sc_incl "firstboot" .
      sc_incl "grub" .
      sc_incl "hsqldb" .
      sc_incl "httpd" .
      sc_incl "i18n" .
      sc_incl "init" .
      sc_incl "iptables-config" .
      sc_incl "irda" .
      sc_incl "irqbalance" .
      sc_incl "kdump" .
      sc_incl "kernel" .
      sc_incl "keyboard" .
      sc_incl "kudzu" .
      sc_incl "libvirtd" .
      sc_incl "lircd" .
      sc_incl "nasd" .
      sc_incl "netconsole" .
      sc_incl "network" .
      sc_incl "nfs" .
      sc_incl "ntpd" .
      sc_incl "prelink" .
      sc_incl "puppet" .
      sc_incl "puppetmaster" .
      sc_incl "readonly-root" .
      sc_incl "rsyslog" .
      sc_incl "samba" .
      sc_incl "saslauthd" .
      sc_incl "selinux" .
      sc_incl "sendmail" .
      sc_incl "smartmontools" .
      sc_incl "snmpd" .
      sc_incl "snmpd.options" .
      sc_incl "snmptrapd" .
      sc_incl "snmptrapd.options" .
      sc_incl "spamassassin" .
      sc_incl "suseconfig" .
      sc_incl "sysstat" .
      sc_incl "system-config-users" .
      sc_incl "vncservers" .
      sc_incl "wpa_supplicant" .
      sc_incl "xend" .
      sc_incl "xendomains"

  let filter = filter_sysconfig
             . Util.stdexcl

  let xfm = transform lns filter

(*
  Examples:

  abc   -> abc -> abc
  "abc" -> abc -> abc
  "a b" -> a b -> "a b"
  'a"b' -> a"b -> 'a"b'
*)
