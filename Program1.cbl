       identification division.
       program-id. ItemListProgram.
       author. Andre Agrippa.
       date-written. 2021-01-21.

      **************************************************************
      *Description: Demonstrate all of the basic file I/O,
      * Takes in item information, performs calculations,
      * and outputs formatted information.
      **************************************************************
      
       
       environment division.
       configuration section.

       input-output section.
       file-control.

           select input-file
               assign to "../../../A2.dat"
               organization is line sequential.
           select output-file
               assign to "../../../A2.out"
               organization is line sequential.

       data division.

       file section.

      *       Input and Output files and record definitions

           fd input-file
               data record is input-line
               record contains 27 characters.

       01 input-line.
         05 il-item-number             pic 9(4).
         05 il-item-class              pic x(1).
         05 il-item-description        pic x(13).
         05 il-item-quantity           pic 999.
         05 il-per-unit-price          pic 9999V99.

           fd output-file
               data record is output-line
               record contains 123 characters.

       01 output-line                  pic x(123).

       working-storage section.

       01 ws-report-heading.
         05 filler                     pic x(105) value spaces.
         05 filler                     pic x(17) value 
         "Andre Agrippa, A2".

       01 ws-column-heading-1.
         05 filler                     pic x(4) value "ITEM".
         05 filler                     pic x(5) value spaces.
         05 filler                     pic x(4) value "ITEM".
         05 filler                     pic x(6) value spaces.
         05 filler                     pic x(3) value "QTY".
         05 filler                     pic x(5) value spaces.
         05 filler                     pic x(4) value "UNIT".
         05 filler                     pic x(10) value spaces.
         05 filler                     pic x(8) value "EXTENDED".
         05 filler                     pic x(7) value spaces.
         05 filler                     pic x(8) value "DISCOUNT".
         05 filler                     pic x(8) value spaces.
         05 filler                     pic x(9) value "NET PRICE".
         05 filler                     pic x(5) value spaces.
         05 filler                     pic x(5) value "CLASS".
         05 filler                     pic x(5) value spaces.
         05 filler                     pic x(5) value "TRANS".
         05 filler                     pic x(7) value spaces.
         05 filler                     pic x(14) value "TRANSPORTATION".

       01 ws-column-heading-2.
         05 filler                     pic x(2) value " #".
         05 filler                     pic x(3) value spaces.
         05 filler                     pic x(13) value "DESCRIPTION".
         05 filler                     pic x(9) value spaces.
         05 filler                     pic x(5) value "PRICE".
         05 filler                     pic x(9) value spaces.
         05 filler                     pic x(5) value "PRICE".
         05 filler                     pic x(10) value spaces.
         05 filler                     pic x(6) value "AMOUNT".
         05 filler                     pic x(36) value spaces.
         05 filler                     pic x value "%".
         05 filler                     pic x(13) value spaces.
         05 filler                     pic x(6) value "CHARGE".

       01 ws-detail-line.
         05 ws-item-number             pic x(4).
         05 filler                     pic x(1) value spaces.
         05 ws-item-description        pic x(13).
         05 filler                     pic x(1) value spaces.
         05 ws-item-quantity           pic ZZZ.
         05 filler                     pic x(3) value spaces.
         05 ws-per-unit-price          pic Z,ZZ9.99.
         05 filler                     pic x(3) value spaces.
         05 ws-extended-price          pic ZZ,ZZZ,ZZ9.99.
         05 filler                     pic x(3) value spaces.
         05 ws-discount-price          pic ZZZ,ZZ9.99.
         05 filler                     pic x(5) value spaces.
         05 ws-net-price               pic ZZ,ZZZ,ZZ9.99.
         05 filler                     pic x(8) value spaces.
         05 ws-product-class           pic x.
         05 filler                     pic x(6) value spaces.
         05 ws-trans-percent  pic ZZ9.9.
         05 ws-percent-sign            pic x.
         05 filler                     pic x(5) value spaces.
         05 ws-trans-charge   pic ZZ,ZZZ,ZZ9.99.
         05 filler                     pic x(5) value spaces.
         05 ws-list-flag               pic x.

       01 ws-calcs.
         05 ws-calcs-quantity          pic 999 value 0.
         05 ws-calcs-per-unit-price    pic 9999v99 value 0.
         05 ws-calcs-extended-price    pic 99999999v99 value 0.
         05 ws-calcs-discount-price    pic 999999v99 value 0.00.
         05 ws-calcs-net-price         pic 99999999v99 value 0.00.
         05 ws-calcs-trans-percent pic 999v9 value 0.00.
         05 ws-calcs-trans-charge  pic 99999999v99 value 0.
         05 ws-calcs-total-extended-price   pic 999999999v99 value 0.
         05 ws-calcs-total-net-price        pic 99999999v99 value 0.
         05 ws-calcs-total-trans-charge     pic 9(8)v99 value 0.
         05 ws-calcs-total-records          pic 999 value 0.
         05 ws-calcs-total-records-discount pic 999 value 0.
         05 ws-calcs-total-records-percent  pic 999v999 value 0.

       01 ws-summary-line.
         05 filler                     pic x(35) value spaces.
         05 sl-total-extended-price    pic $$$,$$$,$$9.99 value 0.
         05 filler                     pic x(18) value spaces.
         05 sl-total-net-price         pic $$,$$$,$$9.99 value 0.
         05 filler                     pic x(26) value spaces.
         05 sl-total-trans-charge      pic $$,$$$,$$9.99.

       01 ws-without-discount.
         05 filler                     pic x(22) value 
         "ITEMS WITHOUT DISCOUNT".
         05 ws-items-without-discount  pic ZZ9.9.
         05 ws-percent-sign-discount   pic x.

       01 ws-flags.
         05 ws-eof-flag                pic x value "n".

       01 ws-cnsts.
         05 ws-percent-sign-cnst       pic x value "%".

       procedure division.
       000-main.

      *
      * Open files
      *
           open input input-file.
           open output output-file.

      *
      * Write the report and column heading
      *
           write output-line from ws-report-heading
             after advancing 1 line.
           write output-line from ws-column-heading-1
             after advancing 3 line.
           write output-line from ws-column-heading-2.

      *
      * Initial read of input file
      *
           read input-file
               at end
                   move "y" to ws-eof-flag.

      *
      * Process each input record, calculate total discount records 
      * percent, write summary line to output line, write items without
      * discount percent.
      *
           perform 100-process-file
             until ws-eof-flag equals "y".

           divide ws-calcs-total-records-discount
             by ws-calcs-total-records
             giving ws-calcs-total-records-percent rounded.

           multiply ws-calcs-total-records-percent by 100
             giving ws-calcs-total-records-percent.

           move ws-calcs-total-records-percent to
             ws-items-without-discount.

           write output-line from ws-summary-line
             after advancing 3 lines.

           move ws-percent-sign to ws-percent-sign-discount.
           write output-line from ws-without-discount
             after advancing 1 lines.

      *
      *  Close files and end program
      *

           close input-file
             output-file.
           accept return-code.
           goback.

       100-process-file.

           add 1 to ws-calcs-total-records.
      *
      *  Calculations extended price
      *
           move il-per-unit-price to ws-calcs-per-unit-price.
           move il-item-quantity to ws-calcs-quantity.

           multiply ws-calcs-quantity by ws-calcs-per-unit-price
             giving ws-calcs-extended-price rounded.

           add ws-calcs-extended-price to ws-calcs-total-extended-price.

      *
      *  Calculations discounted price
      *

           multiply ws-calcs-discount-price by 0
             giving ws-calcs-discount-price.

           if il-item-class = "A" and ws-calcs-extended-price > 100
             then
               multiply ws-calcs-extended-price by 0.05
                 giving ws-calcs-discount-price rounded
           end-if.
           if il-item-class = "F" and ws-calcs-extended-price > 50
             then
               multiply ws-calcs-extended-price by 0.05
                 giving ws-calcs-discount-price rounded
           end-if.
           if il-item-class = "B" and ws-calcs-quantity > 5
             then
               multiply ws-calcs-extended-price by 0.05
                 giving ws-calcs-discount-price rounded
           end-if.

           if ws-calcs-discount-price = 0.00 then
               add 1 to ws-calcs-total-records-discount
           end-if.

      *
      *  Calculations net price
      *
           subtract ws-calcs-discount-price from ws-calcs-extended-price
             giving ws-calcs-net-price.

           add ws-calcs-net-price to
             ws-calcs-total-net-price.

      *
      *  Calculations transportation charge
      *

           if il-item-class = "A"
             then
               move 12.5 to ws-calcs-trans-percent
               multiply ws-calcs-extended-price by 0.125
                 giving ws-calcs-trans-charge rounded
           end-if.
           if il-item-class = "D"
             then
               move 8.5 to ws-calcs-trans-percent
               multiply ws-calcs-extended-price by 0.085
                 giving ws-calcs-trans-charge rounded
           end-if.
           if il-item-class = "F"
             then
               move 4.5 to ws-calcs-trans-percent
               multiply ws-calcs-extended-price by 0.045
                 giving ws-calcs-trans-charge rounded
           end-if.

           if il-item-class not = "A" and
             il-item-class not = "D" and
             il-item-class not = "F" and
             il-item-quantity <= 100
               move 6.5 to ws-calcs-trans-percent
               multiply ws-calcs-extended-price by 0.065
                 giving ws-calcs-trans-charge rounded
           else
               if il-item-class not = "A" and
                 il-item-class not = "D" and
                 il-item-class not = "F" and
                 il-item-quantity > 100
                   move 0.0 to ws-calcs-trans-percent
                   move 45.0 to ws-calcs-trans-charge
               end-if.

           add ws-calcs-trans-charge to
             ws-calcs-total-trans-charge.

      *
      *   Clear the detail line
      *
           move spaces to ws-detail-line.
      *
      *   Move input data to detail line and write to output file
      *
           move il-item-number         to ws-item-number.
           move il-item-description    to ws-item-description.
           move ws-calcs-quantity      to ws-item-quantity.
           move ws-calcs-per-unit-price     to ws-per-unit-price.
           move ws-calcs-extended-price     to ws-extended-price.
           move ws-calcs-discount-price     to ws-discount-price.
           move ws-calcs-net-price          to ws-net-price.
           move ws-calcs-trans-percent to ws-trans-percent.
           move ws-calcs-trans-charge  to ws-trans-charge.
           move ws-percent-sign-cnst   to ws-percent-sign.
           move il-item-class          to ws-product-class.
           move ws-calcs-total-extended-price to
         sl-total-extended-price.
           move ws-calcs-total-net-price    to sl-total-net-price.
           move ws-calcs-total-trans-charge to
             sl-total-trans-charge.

      *
      *   Write details to output-line
      *

           write output-line from ws-detail-line
             after advancing 2 lines.

      *
      *   Read next input record for the next iteration of perform loop
      *

           read input-file
               at end
                   move "y" to ws-eof-flag.

       end program ItemListProgram.