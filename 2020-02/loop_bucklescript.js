// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Block = require("./stdlib/block.js");
var Curry = require("./stdlib/curry.js");
var Format = require("./stdlib/format.js");

function hit(x, y) {
  if (x < 0 && x > -1 && y > 0 && y < 1 || x > 1 && x < 2 && y > 0 && y < 1 || y < 0 && y > -1 && x > 0 && x < 1) {
    return true;
  } else if (y > 1 && y < 2 && x > 0) {
    return x < 1;
  } else {
    return false;
  }
}

function print_header(param) {
  Format.printf(/* Format */[
        /* Char_literal */Block.__(12, [
            /* "\n" */10,
            /* End_of_format */0
          ]),
        "\n"
      ]);
  Format.printf(/* Format */[
        /* String_literal */Block.__(11, [
            "  length   probability     one_line      segments    xn    yn    sn\n",
            /* End_of_format */0
          ]),
        "  length   probability     one_line      segments    xn    yn    sn\n"
      ]);
  return Format.printf(/* Format */[
              /* String_literal */Block.__(11, [
                  " --------- ----------- ------------ ------------- ----- ----- -----\n",
                  /* Flush */Block.__(10, [/* End_of_format */0])
                ]),
              " --------- ----------- ------------ ------------- ----- ----- -----\n%!"
            ]);
}

function print_stats(hit_count, total_count, length, xn, yn, sn) {
  Curry._2(Format.printf(/* Format */[
            /* Float */Block.__(8, [
                /* Float_f */0,
                /* Lit_padding */Block.__(0, [
                    /* Right */1,
                    10
                  ]),
                /* No_precision */0,
                /* Char_literal */Block.__(12, [
                    /* " " */32,
                    /* Float */Block.__(8, [
                        /* Float_f */0,
                        /* Lit_padding */Block.__(0, [
                            /* Right */1,
                            11
                          ]),
                        /* No_precision */0,
                        /* End_of_format */0
                      ])
                  ])
              ]),
            "%10f %11f"
          ]), length, hit_count / total_count);
  return Curry._5(Format.printf(/* Format */[
                  /* Int */Block.__(4, [
                      /* Int_i */3,
                      /* Lit_padding */Block.__(0, [
                          /* Right */1,
                          13
                        ]),
                      /* No_precision */0,
                      /* Char_literal */Block.__(12, [
                          /* " " */32,
                          /* Int */Block.__(4, [
                              /* Int_i */3,
                              /* Lit_padding */Block.__(0, [
                                  /* Right */1,
                                  13
                                ]),
                              /* No_precision */0,
                              /* Char_literal */Block.__(12, [
                                  /* " " */32,
                                  /* Int */Block.__(4, [
                                      /* Int_i */3,
                                      /* Lit_padding */Block.__(0, [
                                          /* Right */1,
                                          5
                                        ]),
                                      /* No_precision */0,
                                      /* Char_literal */Block.__(12, [
                                          /* " " */32,
                                          /* Int */Block.__(4, [
                                              /* Int_i */3,
                                              /* Lit_padding */Block.__(0, [
                                                  /* Right */1,
                                                  5
                                                ]),
                                              /* No_precision */0,
                                              /* Char_literal */Block.__(12, [
                                                  /* " " */32,
                                                  /* Int */Block.__(4, [
                                                      /* Int_i */3,
                                                      /* Lit_padding */Block.__(0, [
                                                          /* Right */1,
                                                          5
                                                        ]),
                                                      /* No_precision */0,
                                                      /* Char_literal */Block.__(12, [
                                                          /* "\n" */10,
                                                          /* Flush */Block.__(10, [/* End_of_format */0])
                                                        ])
                                                    ])
                                                ])
                                            ])
                                        ])
                                    ])
                                ])
                            ])
                        ])
                    ]),
                  "%#13i %#13i %5i %5i %5i\n%!"
                ]), hit_count, total_count, xn, yn, sn);
}

function pol2cart(length, sigma) {
  return /* tuple */[
          length * Math.cos(sigma),
          length * Math.sin(sigma)
        ];
}

function calculate(xn, yn, sn, length) {
  var pi2 = 2 * 3.1415926535897931;
  var total_count = 0;
  var hit_count = 0;
  for(var s = 0 ,s_finish = sn - 1 | 0; s <= s_finish; ++s){
    var sigma = s * pi2 / sn;
    var match = pol2cart(length, sigma);
    var dy = match[1];
    var dx = match[0];
    for(var x = 0 ,x_finish = xn - 1 | 0; x <= x_finish; ++x){
      var x_start = x / xn;
      for(var y = 0 ,y_finish = yn - 1 | 0; y <= y_finish; ++y){
        var y_start = y / yn;
        var x_end = x_start + dx;
        var y_end = y_start + dy;
        if (hit(x_end, y_end)) {
          hit_count = hit_count + 1 | 0;
        }
        total_count = total_count + 1 | 0;
      }
    }
  }
  return print_stats(hit_count, total_count, length, xn, yn, sn);
}

var length = 0.0;

print_header(/* () */0);

while(length < 2.300001) {
  calculate(200, 200, 300, length);
  length = length + 0.1;
};

exports.hit = hit;
exports.print_header = print_header;
exports.print_stats = print_stats;
exports.pol2cart = pol2cart;
exports.calculate = calculate;
/*  Not a pure module */
