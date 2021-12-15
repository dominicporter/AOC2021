module Day5TestData exposing (..)


sampleSegments =
    [ ( ( 0, 9 ), ( 5, 9 ) )
    , ( ( 8, 0 ), ( 0, 8 ) )
    , ( ( 9, 4 ), ( 3, 4 ) )
    , ( ( 2, 2 ), ( 2, 1 ) )
    , ( ( 7, 0 ), ( 7, 4 ) )
    , ( ( 6, 4 ), ( 2, 0 ) )
    , ( ( 0, 9 ), ( 2, 9 ) )
    , ( ( 3, 4 ), ( 1, 4 ) )
    , ( ( 0, 0 ), ( 8, 8 ) )
    , ( ( 5, 5 ), ( 8, 2 ) )
    ]

sampleFinalBoard = 
  [
  [0,0,0,0,0,0,0,1,0,0],
  [0,0,1,0,0,0,0,1,0,0],
  [0,0,1,0,0,0,0,1,0,0],
  [0,0,0,0,0,0,0,1,0,0],
  [0,1,1,2,1,1,1,2,1,1],
  [0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0],
  [2,2,2,1,1,1,0,0,0,0]
  ]

actualSegments =
    [ ( ( 60, 28 ), ( 893, 861 ) )
    , ( ( 934, 945 ), ( 222, 233 ) )
    , ( ( 125, 246 ), ( 125, 306 ) )
    , ( ( 490, 255 ), ( 490, 847 ) )
    , ( ( 457, 868 ), ( 364, 961 ) )
    , ( ( 610, 46 ), ( 610, 826 ) )
    , ( ( 338, 711 ), ( 982, 67 ) )
    , ( ( 199, 581 ), ( 295, 581 ) )
    , ( ( 578, 489 ), ( 522, 545 ) )
    , ( ( 180, 516 ), ( 180, 904 ) )
    , ( ( 354, 363 ), ( 145, 363 ) )
    , ( ( 757, 471 ), ( 692, 471 ) )
    , ( ( 896, 71 ), ( 896, 185 ) )
    , ( ( 967, 744 ), ( 967, 486 ) )
    , ( ( 166, 19 ), ( 483, 19 ) )
    , ( ( 469, 22 ), ( 529, 22 ) )
    , ( ( 774, 311 ), ( 807, 311 ) )
    , ( ( 617, 308 ), ( 203, 308 ) )
    , ( ( 694, 405 ), ( 694, 43 ) )
    , ( ( 378, 176 ), ( 378, 488 ) )
    , ( ( 989, 189 ), ( 215, 189 ) )
    , ( ( 375, 96 ), ( 612, 96 ) )
    , ( ( 505, 467 ), ( 505, 246 ) )
    , ( ( 77, 832 ), ( 77, 473 ) )
    , ( ( 686, 879 ), ( 684, 879 ) )
    , ( ( 360, 593 ), ( 151, 384 ) )
    , ( ( 387, 322 ), ( 626, 322 ) )
    , ( ( 66, 784 ), ( 66, 109 ) )
    , ( ( 100, 411 ), ( 635, 946 ) )
    , ( ( 722, 14 ), ( 722, 784 ) )
    , ( ( 724, 751 ), ( 764, 751 ) )
    , ( ( 788, 844 ), ( 32, 88 ) )
    , ( ( 905, 799 ), ( 905, 713 ) )
    , ( ( 282, 502 ), ( 238, 502 ) )
    , ( ( 685, 259 ), ( 685, 768 ) )
    , ( ( 116, 578 ), ( 477, 217 ) )
    , ( ( 115, 78 ), ( 115, 458 ) )
    , ( ( 329, 569 ), ( 66, 306 ) )
    , ( ( 817, 815 ), ( 817, 466 ) )
    , ( ( 781, 909 ), ( 872, 909 ) )
    , ( ( 62, 44 ), ( 964, 946 ) )
    , ( ( 755, 307 ), ( 593, 307 ) )
    , ( ( 436, 56 ), ( 436, 869 ) )
    , ( ( 596, 815 ), ( 49, 268 ) )
    , ( ( 465, 986 ), ( 926, 525 ) )
    , ( ( 885, 254 ), ( 39, 254 ) )
    , ( ( 947, 433 ), ( 764, 433 ) )
    , ( ( 719, 787 ), ( 200, 787 ) )
    , ( ( 380, 461 ), ( 243, 461 ) )
    , ( ( 675, 434 ), ( 675, 582 ) )
    , ( ( 106, 548 ), ( 272, 714 ) )
    , ( ( 703, 143 ), ( 703, 111 ) )
    , ( ( 238, 745 ), ( 60, 745 ) )
    , ( ( 646, 235 ), ( 646, 742 ) )
    , ( ( 243, 439 ), ( 243, 964 ) )
    , ( ( 347, 763 ), ( 321, 789 ) )
    , ( ( 322, 294 ), ( 738, 294 ) )
    , ( ( 134, 361 ), ( 946, 361 ) )
    , ( ( 223, 30 ), ( 498, 305 ) )
    , ( ( 78, 721 ), ( 288, 721 ) )
    , ( ( 792, 875 ), ( 572, 875 ) )
    , ( ( 548, 380 ), ( 637, 291 ) )
    , ( ( 85, 417 ), ( 85, 296 ) )
    , ( ( 766, 81 ), ( 766, 131 ) )
    , ( ( 340, 218 ), ( 340, 271 ) )
    , ( ( 56, 962 ), ( 974, 44 ) )
    , ( ( 415, 940 ), ( 386, 940 ) )
    , ( ( 960, 60 ), ( 46, 974 ) )
    , ( ( 719, 527 ), ( 743, 527 ) )
    , ( ( 971, 986 ), ( 313, 986 ) )
    , ( ( 415, 316 ), ( 415, 57 ) )
    , ( ( 612, 556 ), ( 612, 648 ) )
    , ( ( 421, 776 ), ( 570, 776 ) )
    , ( ( 478, 533 ), ( 478, 831 ) )
    , ( ( 758, 304 ), ( 112, 950 ) )
    , ( ( 258, 950 ), ( 79, 950 ) )
    , ( ( 329, 349 ), ( 216, 349 ) )
    , ( ( 527, 38 ), ( 755, 38 ) )
    , ( ( 517, 239 ), ( 207, 239 ) )
    , ( ( 497, 944 ), ( 898, 944 ) )
    , ( ( 418, 642 ), ( 418, 557 ) )
    , ( ( 828, 750 ), ( 456, 750 ) )
    , ( ( 632, 916 ), ( 15, 299 ) )
    , ( ( 955, 973 ), ( 970, 958 ) )
    , ( ( 474, 524 ), ( 949, 49 ) )
    , ( ( 842, 690 ), ( 842, 116 ) )
    , ( ( 203, 267 ), ( 916, 980 ) )
    , ( ( 128, 562 ), ( 936, 562 ) )
    , ( ( 682, 963 ), ( 193, 963 ) )
    , ( ( 704, 822 ), ( 44, 162 ) )
    , ( ( 371, 309 ), ( 421, 359 ) )
    , ( ( 379, 240 ), ( 510, 240 ) )
    , ( ( 547, 177 ), ( 547, 20 ) )
    , ( ( 784, 968 ), ( 784, 613 ) )
    , ( ( 75, 138 ), ( 75, 724 ) )
    , ( ( 725, 48 ), ( 32, 741 ) )
    , ( ( 141, 674 ), ( 545, 674 ) )
    , ( ( 193, 922 ), ( 193, 845 ) )
    , ( ( 916, 831 ), ( 916, 401 ) )
    , ( ( 912, 232 ), ( 923, 232 ) )
    , ( ( 127, 911 ), ( 911, 127 ) )
    , ( ( 140, 381 ), ( 140, 913 ) )
    , ( ( 472, 243 ), ( 472, 134 ) )
    , ( ( 311, 548 ), ( 311, 741 ) )
    , ( ( 415, 590 ), ( 415, 409 ) )
    , ( ( 20, 984 ), ( 983, 21 ) )
    , ( ( 572, 575 ), ( 572, 756 ) )
    , ( ( 804, 188 ), ( 20, 972 ) )
    , ( ( 485, 104 ), ( 962, 104 ) )
    , ( ( 31, 966 ), ( 986, 11 ) )
    , ( ( 584, 473 ), ( 338, 473 ) )
    , ( ( 974, 962 ), ( 34, 22 ) )
    , ( ( 176, 279 ), ( 921, 279 ) )
    , ( ( 305, 863 ), ( 305, 981 ) )
    , ( ( 137, 889 ), ( 137, 47 ) )
    , ( ( 888, 259 ), ( 888, 125 ) )
    , ( ( 794, 774 ), ( 794, 455 ) )
    , ( ( 193, 118 ), ( 966, 118 ) )
    , ( ( 429, 302 ), ( 547, 184 ) )
    , ( ( 59, 49 ), ( 984, 974 ) )
    , ( ( 978, 56 ), ( 232, 802 ) )
    , ( ( 23, 142 ), ( 456, 142 ) )
    , ( ( 532, 574 ), ( 532, 265 ) )
    , ( ( 936, 263 ), ( 933, 263 ) )
    , ( ( 773, 856 ), ( 230, 313 ) )
    , ( ( 182, 809 ), ( 913, 809 ) )
    , ( ( 364, 958 ), ( 901, 958 ) )
    , ( ( 724, 290 ), ( 64, 950 ) )
    , ( ( 312, 967 ), ( 312, 166 ) )
    , ( ( 208, 286 ), ( 566, 286 ) )
    , ( ( 828, 907 ), ( 828, 729 ) )
    , ( ( 79, 692 ), ( 753, 18 ) )
    , ( ( 235, 601 ), ( 811, 601 ) )
    , ( ( 735, 206 ), ( 301, 206 ) )
    , ( ( 246, 112 ), ( 246, 423 ) )
    , ( ( 712, 439 ), ( 712, 108 ) )
    , ( ( 570, 179 ), ( 751, 179 ) )
    , ( ( 766, 22 ), ( 816, 22 ) )
    , ( ( 51, 686 ), ( 27, 686 ) )
    , ( ( 50, 954 ), ( 50, 31 ) )
    , ( ( 762, 413 ), ( 762, 601 ) )
    , ( ( 223, 812 ), ( 670, 812 ) )
    , ( ( 391, 882 ), ( 712, 882 ) )
    , ( ( 842, 332 ), ( 922, 332 ) )
    , ( ( 540, 88 ), ( 540, 124 ) )
    , ( ( 75, 312 ), ( 161, 312 ) )
    , ( ( 979, 984 ), ( 10, 15 ) )
    , ( ( 479, 856 ), ( 823, 856 ) )
    , ( ( 690, 491 ), ( 298, 883 ) )
    , ( ( 481, 401 ), ( 481, 279 ) )
    , ( ( 870, 942 ), ( 276, 348 ) )
    , ( ( 39, 935 ), ( 937, 37 ) )
    , ( ( 706, 275 ), ( 706, 948 ) )
    , ( ( 530, 892 ), ( 535, 897 ) )
    , ( ( 743, 223 ), ( 929, 223 ) )
    , ( ( 682, 917 ), ( 490, 917 ) )
    , ( ( 616, 268 ), ( 456, 268 ) )
    , ( ( 484, 72 ), ( 429, 72 ) )
    , ( ( 61, 365 ), ( 430, 365 ) )
    , ( ( 382, 741 ), ( 910, 741 ) )
    , ( ( 710, 406 ), ( 330, 406 ) )
    , ( ( 795, 770 ), ( 55, 770 ) )
    , ( ( 117, 416 ), ( 352, 651 ) )
    , ( ( 593, 151 ), ( 20, 724 ) )
    , ( ( 238, 556 ), ( 584, 556 ) )
    , ( ( 680, 583 ), ( 680, 504 ) )
    , ( ( 678, 440 ), ( 212, 440 ) )
    , ( ( 508, 222 ), ( 508, 844 ) )
    , ( ( 435, 873 ), ( 93, 873 ) )
    , ( ( 129, 607 ), ( 468, 268 ) )
    , ( ( 280, 147 ), ( 94, 147 ) )
    , ( ( 238, 872 ), ( 971, 139 ) )
    , ( ( 881, 339 ), ( 664, 339 ) )
    , ( ( 289, 960 ), ( 289, 664 ) )
    , ( ( 70, 762 ), ( 973, 762 ) )
    , ( ( 429, 24 ), ( 429, 202 ) )
    , ( ( 907, 785 ), ( 907, 190 ) )
    , ( ( 598, 548 ), ( 598, 63 ) )
    , ( ( 324, 220 ), ( 281, 220 ) )
    , ( ( 754, 980 ), ( 79, 980 ) )
    , ( ( 568, 508 ), ( 583, 508 ) )
    , ( ( 364, 712 ), ( 503, 712 ) )
    , ( ( 655, 963 ), ( 898, 963 ) )
    , ( ( 253, 359 ), ( 46, 566 ) )
    , ( ( 989, 989 ), ( 14, 14 ) )
    , ( ( 329, 924 ), ( 380, 924 ) )
    , ( ( 248, 826 ), ( 675, 826 ) )
    , ( ( 417, 428 ), ( 417, 320 ) )
    , ( ( 13, 12 ), ( 984, 983 ) )
    , ( ( 916, 53 ), ( 916, 896 ) )
    , ( ( 247, 285 ), ( 377, 155 ) )
    , ( ( 937, 588 ), ( 710, 588 ) )
    , ( ( 473, 270 ), ( 466, 277 ) )
    , ( ( 567, 74 ), ( 567, 388 ) )
    , ( ( 371, 470 ), ( 228, 470 ) )
    , ( ( 640, 96 ), ( 766, 96 ) )
    , ( ( 725, 499 ), ( 372, 499 ) )
    , ( ( 184, 561 ), ( 184, 236 ) )
    , ( ( 654, 446 ), ( 933, 446 ) )
    , ( ( 156, 153 ), ( 978, 975 ) )
    , ( ( 811, 228 ), ( 922, 339 ) )
    , ( ( 84, 861 ), ( 878, 67 ) )
    , ( ( 622, 329 ), ( 622, 425 ) )
    , ( ( 415, 186 ), ( 450, 221 ) )
    , ( ( 109, 488 ), ( 653, 488 ) )
    , ( ( 982, 16 ), ( 33, 965 ) )
    , ( ( 100, 885 ), ( 829, 156 ) )
    , ( ( 342, 914 ), ( 342, 636 ) )
    , ( ( 177, 323 ), ( 728, 874 ) )
    , ( ( 81, 414 ), ( 406, 739 ) )
    , ( ( 889, 79 ), ( 889, 698 ) )
    , ( ( 504, 450 ), ( 148, 806 ) )
    , ( ( 961, 33 ), ( 51, 943 ) )
    , ( ( 656, 21 ), ( 100, 21 ) )
    , ( ( 32, 60 ), ( 32, 562 ) )
    , ( ( 499, 174 ), ( 499, 301 ) )
    , ( ( 162, 740 ), ( 162, 906 ) )
    , ( ( 190, 183 ), ( 811, 804 ) )
    , ( ( 93, 960 ), ( 13, 960 ) )
    , ( ( 787, 681 ), ( 866, 681 ) )
    , ( ( 254, 332 ), ( 254, 79 ) )
    , ( ( 595, 873 ), ( 595, 496 ) )
    , ( ( 151, 737 ), ( 151, 390 ) )
    , ( ( 974, 429 ), ( 990, 429 ) )
    , ( ( 295, 784 ), ( 295, 513 ) )
    , ( ( 378, 942 ), ( 378, 283 ) )
    , ( ( 152, 838 ), ( 796, 838 ) )
    , ( ( 624, 630 ), ( 881, 887 ) )
    , ( ( 90, 420 ), ( 412, 420 ) )
    , ( ( 868, 69 ), ( 46, 891 ) )
    , ( ( 75, 890 ), ( 452, 513 ) )
    , ( ( 133, 460 ), ( 133, 985 ) )
    , ( ( 970, 145 ), ( 549, 145 ) )
    , ( ( 149, 462 ), ( 916, 462 ) )
    , ( ( 92, 845 ), ( 92, 268 ) )
    , ( ( 580, 99 ), ( 250, 99 ) )
    , ( ( 618, 708 ), ( 618, 652 ) )
    , ( ( 690, 948 ), ( 690, 38 ) )
    , ( ( 808, 594 ), ( 944, 730 ) )
    , ( ( 100, 359 ), ( 312, 359 ) )
    , ( ( 546, 392 ), ( 41, 897 ) )
    , ( ( 593, 413 ), ( 593, 892 ) )
    , ( ( 602, 484 ), ( 602, 144 ) )
    , ( ( 90, 863 ), ( 90, 170 ) )
    , ( ( 888, 987 ), ( 888, 162 ) )
    , ( ( 229, 932 ), ( 960, 201 ) )
    , ( ( 919, 654 ), ( 70, 654 ) )
    , ( ( 13, 684 ), ( 13, 348 ) )
    , ( ( 743, 477 ), ( 166, 477 ) )
    , ( ( 901, 113 ), ( 936, 113 ) )
    , ( ( 167, 567 ), ( 540, 567 ) )
    , ( ( 566, 729 ), ( 566, 660 ) )
    , ( ( 102, 660 ), ( 615, 660 ) )
    , ( ( 273, 241 ), ( 273, 413 ) )
    , ( ( 512, 241 ), ( 512, 643 ) )
    , ( ( 869, 695 ), ( 614, 440 ) )
    , ( ( 356, 583 ), ( 356, 408 ) )
    , ( ( 61, 345 ), ( 61, 233 ) )
    , ( ( 973, 33 ), ( 88, 918 ) )
    , ( ( 977, 130 ), ( 771, 130 ) )
    , ( ( 422, 382 ), ( 899, 382 ) )
    , ( ( 536, 517 ), ( 914, 139 ) )
    , ( ( 563, 755 ), ( 312, 755 ) )
    , ( ( 770, 581 ), ( 770, 940 ) )
    , ( ( 103, 186 ), ( 313, 186 ) )
    , ( ( 681, 490 ), ( 77, 490 ) )
    , ( ( 676, 351 ), ( 913, 588 ) )
    , ( ( 292, 700 ), ( 862, 700 ) )
    , ( ( 445, 175 ), ( 188, 175 ) )
    , ( ( 62, 490 ), ( 173, 601 ) )
    , ( ( 530, 455 ), ( 63, 455 ) )
    , ( ( 145, 85 ), ( 832, 772 ) )
    , ( ( 273, 414 ), ( 273, 240 ) )
    , ( ( 25, 888 ), ( 25, 684 ) )
    , ( ( 599, 393 ), ( 599, 232 ) )
    , ( ( 198, 296 ), ( 584, 682 ) )
    , ( ( 217, 886 ), ( 614, 886 ) )
    , ( ( 464, 598 ), ( 362, 496 ) )
    , ( ( 874, 106 ), ( 874, 227 ) )
    , ( ( 248, 511 ), ( 940, 511 ) )
    , ( ( 501, 861 ), ( 21, 381 ) )
    , ( ( 385, 232 ), ( 341, 232 ) )
    , ( ( 258, 449 ), ( 337, 449 ) )
    , ( ( 94, 46 ), ( 910, 862 ) )
    , ( ( 946, 825 ), ( 946, 341 ) )
    , ( ( 93, 836 ), ( 93, 781 ) )
    , ( ( 170, 903 ), ( 616, 457 ) )
    , ( ( 717, 333 ), ( 717, 238 ) )
    , ( ( 404, 243 ), ( 516, 243 ) )
    , ( ( 611, 579 ), ( 217, 973 ) )
    , ( ( 76, 851 ), ( 76, 255 ) )
    , ( ( 181, 780 ), ( 661, 780 ) )
    , ( ( 316, 188 ), ( 333, 188 ) )
    , ( ( 799, 92 ), ( 779, 92 ) )
    , ( ( 955, 374 ), ( 869, 374 ) )
    , ( ( 872, 792 ), ( 280, 200 ) )
    , ( ( 337, 239 ), ( 438, 239 ) )
    , ( ( 424, 706 ), ( 273, 857 ) )
    , ( ( 501, 239 ), ( 684, 239 ) )
    , ( ( 198, 671 ), ( 882, 671 ) )
    , ( ( 790, 775 ), ( 802, 775 ) )
    , ( ( 708, 624 ), ( 361, 277 ) )
    , ( ( 547, 731 ), ( 547, 621 ) )
    , ( ( 264, 449 ), ( 293, 449 ) )
    , ( ( 496, 870 ), ( 496, 396 ) )
    , ( ( 988, 959 ), ( 988, 285 ) )
    , ( ( 19, 51 ), ( 926, 958 ) )
    , ( ( 472, 537 ), ( 127, 882 ) )
    , ( ( 188, 488 ), ( 478, 198 ) )
    , ( ( 949, 376 ), ( 797, 224 ) )
    , ( ( 448, 609 ), ( 348, 609 ) )
    , ( ( 838, 285 ), ( 838, 865 ) )
    , ( ( 796, 142 ), ( 70, 868 ) )
    , ( ( 848, 91 ), ( 972, 91 ) )
    , ( ( 722, 964 ), ( 722, 409 ) )
    , ( ( 313, 156 ), ( 313, 725 ) )
    , ( ( 925, 251 ), ( 925, 687 ) )
    , ( ( 803, 815 ), ( 113, 125 ) )
    , ( ( 505, 517 ), ( 505, 337 ) )
    , ( ( 935, 920 ), ( 235, 920 ) )
    , ( ( 674, 274 ), ( 63, 885 ) )
    , ( ( 458, 981 ), ( 626, 981 ) )
    , ( ( 928, 950 ), ( 836, 950 ) )
    , ( ( 163, 453 ), ( 695, 985 ) )
    , ( ( 57, 374 ), ( 398, 374 ) )
    , ( ( 937, 327 ), ( 937, 811 ) )
    , ( ( 975, 932 ), ( 265, 222 ) )
    , ( ( 490, 583 ), ( 490, 482 ) )
    , ( ( 170, 183 ), ( 196, 183 ) )
    , ( ( 738, 978 ), ( 738, 812 ) )
    , ( ( 914, 170 ), ( 914, 202 ) )
    , ( ( 202, 885 ), ( 499, 885 ) )
    , ( ( 270, 887 ), ( 150, 887 ) )
    , ( ( 447, 783 ), ( 831, 399 ) )
    , ( ( 66, 136 ), ( 77, 136 ) )
    , ( ( 536, 703 ), ( 662, 829 ) )
    , ( ( 297, 821 ), ( 297, 792 ) )
    , ( ( 640, 572 ), ( 321, 572 ) )
    , ( ( 244, 833 ), ( 865, 212 ) )
    , ( ( 454, 672 ), ( 454, 726 ) )
    , ( ( 133, 812 ), ( 303, 642 ) )
    , ( ( 280, 589 ), ( 184, 589 ) )
    , ( ( 977, 572 ), ( 977, 42 ) )
    , ( ( 62, 247 ), ( 215, 247 ) )
    , ( ( 427, 503 ), ( 809, 885 ) )
    , ( ( 671, 85 ), ( 671, 770 ) )
    , ( ( 296, 990 ), ( 296, 558 ) )
    , ( ( 103, 19 ), ( 971, 887 ) )
    , ( ( 263, 712 ), ( 263, 329 ) )
    , ( ( 954, 897 ), ( 954, 41 ) )
    , ( ( 278, 536 ), ( 278, 346 ) )
    , ( ( 270, 620 ), ( 983, 620 ) )
    , ( ( 229, 863 ), ( 91, 863 ) )
    , ( ( 935, 413 ), ( 394, 413 ) )
    , ( ( 709, 668 ), ( 77, 668 ) )
    , ( ( 310, 853 ), ( 310, 286 ) )
    , ( ( 534, 694 ), ( 511, 717 ) )
    , ( ( 349, 726 ), ( 349, 439 ) )
    , ( ( 113, 196 ), ( 970, 196 ) )
    , ( ( 836, 340 ), ( 709, 340 ) )
    , ( ( 38, 485 ), ( 38, 14 ) )
    , ( ( 38, 278 ), ( 569, 278 ) )
    , ( ( 862, 90 ), ( 281, 671 ) )
    , ( ( 677, 124 ), ( 405, 124 ) )
    , ( ( 399, 568 ), ( 536, 705 ) )
    , ( ( 611, 839 ), ( 188, 416 ) )
    , ( ( 570, 925 ), ( 570, 251 ) )
    , ( ( 804, 368 ), ( 284, 888 ) )
    , ( ( 262, 842 ), ( 388, 842 ) )
    , ( ( 751, 800 ), ( 751, 504 ) )
    , ( ( 762, 882 ), ( 201, 321 ) )
    , ( ( 411, 421 ), ( 807, 421 ) )
    , ( ( 654, 406 ), ( 265, 795 ) )
    , ( ( 863, 558 ), ( 625, 320 ) )
    , ( ( 451, 673 ), ( 451, 354 ) )
    , ( ( 359, 239 ), ( 566, 239 ) )
    , ( ( 259, 211 ), ( 955, 907 ) )
    , ( ( 253, 506 ), ( 542, 217 ) )
    , ( ( 547, 794 ), ( 373, 620 ) )
    , ( ( 132, 263 ), ( 581, 712 ) )
    , ( ( 168, 237 ), ( 168, 142 ) )
    , ( ( 834, 296 ), ( 152, 978 ) )
    , ( ( 156, 14 ), ( 955, 14 ) )
    , ( ( 927, 22 ), ( 285, 664 ) )
    , ( ( 384, 291 ), ( 362, 269 ) )
    , ( ( 91, 561 ), ( 91, 19 ) )
    , ( ( 472, 953 ), ( 472, 576 ) )
    , ( ( 700, 666 ), ( 723, 689 ) )
    , ( ( 447, 815 ), ( 566, 815 ) )
    , ( ( 698, 411 ), ( 698, 762 ) )
    , ( ( 427, 606 ), ( 119, 298 ) )
    , ( ( 531, 401 ), ( 669, 263 ) )
    , ( ( 681, 21 ), ( 681, 111 ) )
    , ( ( 168, 360 ), ( 168, 447 ) )
    , ( ( 74, 67 ), ( 717, 67 ) )
    , ( ( 287, 88 ), ( 345, 88 ) )
    , ( ( 234, 80 ), ( 234, 848 ) )
    , ( ( 583, 251 ), ( 33, 251 ) )
    , ( ( 200, 522 ), ( 366, 356 ) )
    , ( ( 815, 936 ), ( 27, 148 ) )
    , ( ( 139, 302 ), ( 139, 768 ) )
    , ( ( 473, 69 ), ( 473, 664 ) )
    , ( ( 42, 813 ), ( 42, 918 ) )
    , ( ( 881, 188 ), ( 881, 345 ) )
    , ( ( 457, 920 ), ( 301, 764 ) )
    , ( ( 894, 662 ), ( 779, 662 ) )
    , ( ( 750, 411 ), ( 750, 368 ) )
    , ( ( 986, 167 ), ( 246, 167 ) )
    , ( ( 914, 418 ), ( 742, 590 ) )
    , ( ( 710, 110 ), ( 63, 757 ) )
    , ( ( 353, 493 ), ( 353, 473 ) )
    , ( ( 211, 700 ), ( 181, 700 ) )
    , ( ( 492, 604 ), ( 25, 604 ) )
    , ( ( 212, 174 ), ( 362, 174 ) )
    , ( ( 801, 434 ), ( 752, 385 ) )
    , ( ( 956, 861 ), ( 469, 374 ) )
    , ( ( 197, 318 ), ( 257, 378 ) )
    , ( ( 604, 594 ), ( 604, 809 ) )
    , ( ( 716, 447 ), ( 306, 857 ) )
    , ( ( 974, 974 ), ( 24, 24 ) )
    , ( ( 925, 467 ), ( 925, 311 ) )
    , ( ( 357, 381 ), ( 769, 381 ) )
    , ( ( 714, 395 ), ( 372, 395 ) )
    , ( ( 360, 718 ), ( 728, 718 ) )
    , ( ( 161, 186 ), ( 730, 755 ) )
    , ( ( 407, 316 ), ( 407, 61 ) )
    , ( ( 466, 214 ), ( 333, 347 ) )
    , ( ( 190, 955 ), ( 190, 678 ) )
    , ( ( 969, 48 ), ( 72, 945 ) )
    , ( ( 296, 153 ), ( 833, 153 ) )
    , ( ( 930, 400 ), ( 637, 400 ) )
    , ( ( 606, 953 ), ( 541, 953 ) )
    , ( ( 978, 179 ), ( 21, 179 ) )
    , ( ( 112, 49 ), ( 112, 793 ) )
    , ( ( 346, 881 ), ( 151, 881 ) )
    , ( ( 737, 404 ), ( 737, 693 ) )
    , ( ( 98, 271 ), ( 98, 144 ) )
    , ( ( 469, 830 ), ( 46, 830 ) )
    , ( ( 246, 651 ), ( 246, 243 ) )
    , ( ( 47, 129 ), ( 880, 962 ) )
    , ( ( 449, 609 ), ( 980, 78 ) )
    , ( ( 603, 307 ), ( 603, 896 ) )
    , ( ( 121, 339 ), ( 22, 240 ) )
    , ( ( 97, 726 ), ( 274, 726 ) )
    , ( ( 527, 668 ), ( 786, 409 ) )
    , ( ( 649, 162 ), ( 321, 162 ) )
    , ( ( 253, 10 ), ( 253, 690 ) )
    , ( ( 43, 748 ), ( 590, 748 ) )
    , ( ( 245, 424 ), ( 245, 495 ) )
    , ( ( 509, 595 ), ( 261, 843 ) )
    , ( ( 924, 758 ), ( 683, 758 ) )
    , ( ( 693, 516 ), ( 684, 507 ) )
    , ( ( 654, 201 ), ( 654, 840 ) )
    , ( ( 321, 543 ), ( 315, 549 ) )
    , ( ( 840, 309 ), ( 764, 233 ) )
    , ( ( 865, 183 ), ( 177, 871 ) )
    , ( ( 700, 135 ), ( 14, 821 ) )
    , ( ( 178, 178 ), ( 971, 971 ) )
    , ( ( 88, 649 ), ( 88, 899 ) )
    , ( ( 327, 37 ), ( 327, 58 ) )
    , ( ( 252, 687 ), ( 482, 457 ) )
    , ( ( 12, 771 ), ( 754, 29 ) )
    , ( ( 309, 695 ), ( 630, 695 ) )
    , ( ( 146, 671 ), ( 146, 695 ) )
    , ( ( 226, 697 ), ( 798, 697 ) )
    , ( ( 239, 736 ), ( 239, 945 ) )
    , ( ( 483, 756 ), ( 483, 965 ) )
    , ( ( 306, 475 ), ( 800, 969 ) )
    , ( ( 580, 927 ), ( 580, 102 ) )
    , ( ( 867, 83 ), ( 830, 83 ) )
    , ( ( 635, 359 ), ( 761, 233 ) )
    , ( ( 733, 851 ), ( 180, 298 ) )
    , ( ( 478, 76 ), ( 401, 76 ) )
    , ( ( 552, 581 ), ( 552, 525 ) )
    , ( ( 842, 724 ), ( 847, 724 ) )
    , ( ( 652, 76 ), ( 385, 76 ) )
    , ( ( 695, 894 ), ( 245, 894 ) )
    , ( ( 301, 487 ), ( 301, 665 ) )
    , ( ( 412, 555 ), ( 412, 80 ) )
    , ( ( 591, 311 ), ( 289, 311 ) )
    , ( ( 961, 933 ), ( 69, 41 ) )
    , ( ( 78, 266 ), ( 14, 202 ) )
    , ( ( 255, 696 ), ( 766, 696 ) )
    , ( ( 715, 246 ), ( 508, 246 ) )
    , ( ( 756, 567 ), ( 188, 567 ) )
    , ( ( 866, 377 ), ( 652, 591 ) )
    , ( ( 267, 226 ), ( 204, 163 ) )
    , ( ( 506, 104 ), ( 506, 587 ) )
    , ( ( 270, 434 ), ( 270, 395 ) )
    , ( ( 879, 127 ), ( 879, 859 ) )
    , ( ( 65, 669 ), ( 65, 747 ) )
    , ( ( 486, 745 ), ( 612, 745 ) )
    , ( ( 276, 246 ), ( 276, 41 ) )
    , ( ( 41, 840 ), ( 226, 655 ) )
    , ( ( 207, 495 ), ( 94, 495 ) )
    , ( ( 142, 970 ), ( 285, 970 ) )
    , ( ( 73, 239 ), ( 83, 239 ) )
    , ( ( 787, 409 ), ( 527, 409 ) )
    , ( ( 678, 565 ), ( 678, 582 ) )
    , ( ( 314, 185 ), ( 67, 185 ) )
    ]
