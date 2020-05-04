!>@file   t_control_elements.f90
!!        module t_control_elements
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!!
!>@brief  Structure for reading control items
!!
!!@verbatim
!!      subroutine read_real_ctl_type(c_buf, label, real_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real_item), intent(inout) :: real_item
!!      subroutine read_integer_ctl_type(c_buf, label, int_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_integer_item), intent(inout) :: int_item
!!      subroutine read_chara_ctl_type(c_buf, label, chara_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_character_item), intent(inout) :: chara_item
!!      subroutine read_real2_ctl_type(c_buf, label, real2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real2_item), intent(inout) :: real2_item
!!      subroutine read_real3_ctl_type(c_buf, label, real3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_real3_item), intent(inout) :: real3_item
!!      subroutine read_integer2_ctl_type(c_buf, label, int2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_item), intent(inout) :: int2_item
!!      subroutine read_character2_ctl_type(c_buf, label, chara2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara2_item), intent(inout) :: chara2_item
!!      subroutine read_character3_ctl_type(c_buf, label, chara3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara3_item), intent(inout) :: chara3_item
!!      subroutine read_charreal2_ctl_type(c_buf, label, cr2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_real2_item), intent(inout) :: cr2_item
!!      subroutine read_char2real_ctl_type(c_buf, label, c2r_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara2_real_item), intent(inout) :: c2r_item
!!      subroutine read_charareal_ctl_type(c_buf, label, cr_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_real_item), intent(inout) :: cr_item
!!      subroutine read_charaint_ctl_type(c_buf, label, ci_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_int_item), intent(inout) :: ci_item
!!      subroutine read_intchrreal_ctl_type(c_buf, label, icr_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int_chara_real_item), intent(inout) :: icr_item
!!      subroutine read_intreal_ctl_type(c_buf, label, ir_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int_real_item), intent(inout) :: ir_item
!!      subroutine read_int2real_ctl_type(c_buf, label, i2r_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_real_item), intent(inout) :: i2r_item
!!      subroutine read_int2real2_ctl_type(c_buf, label, i2r2_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_int2_real2_item), intent(inout) :: i2r2_item
!!      subroutine read_charaint3_ctl_type(c_buf, label, ci3_item)
!!        type(buffer_for_control), intent(in)  :: c_buf
!!        type(read_chara_int3_item), intent(inout) :: ci3_item
!!
!!      subroutine write_real_ctl_type                                  &
!!     &         (id_file, level, maxlen, label, real_item)
!!        type(read_real_item), intent(in) :: real_item
!!      subroutine write_integer_ctl_type                               &
!!     &         (id_file, level, maxlen, label, int_item)
!!        type(read_integer_item), intent(in) :: int_item
!!      subroutine write_chara_ctl_type                                 &
!!     &         (id_file, level, maxlen, label, chara_item)
!!        type(read_character_item), intent(in) :: chara_item
!!      subroutine write_real2_ctl_type                                 &
!!     &         (id_file, level, label, real2_item)
!!        type(read_real2_item), intent(in) :: real2_item
!!      subroutine write_real3_ctl_type                                 &
!!     &         (id_file, level, label, real3_item)
!!        type(read_real3_item), intent(in) :: real3_item
!!      subroutine write_character3_ctl_type                            &
!!     &         (id_file, level, label, chara3_item)
!!        type(read_chara3_item), intent(in) :: chara3_item
!!      subroutine write_charaint3_ctl_type                             &
!!     &         (id_file, level, label, ci3_item)
!!        type(read_chara_int3_item), intent(in) :: ci3_item
!!@endverbatim
!!
!!@n @param  label   Read label for control items
!
      module t_control_elements
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
!
      implicit none
!
!
!>        structure of control real item
      type read_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_real_item
!
!>        structure of control integer item
      type read_integer_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer item
        integer(kind = kint) ::  intvalue
      end type read_integer_item
!
!>        structure of control item with two reals
      type read_real2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real items
        real(kind = kreal) ::    realvalue(2)
      end type read_real2_item
!
!
!>        structure of control item with three reals
      type read_real3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read real items
        real(kind = kreal) ::    realvalue(3)
      end type read_real3_item
!
!>        structure of control integer item
      type read_int2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer item
        integer(kind = kint) ::  intvalue(2)
      end type read_int2_item
!
!>        structure of control character item
      type read_character_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character item
        character(len=kchara) :: charavalue
      end type read_character_item
!
!>        structure of control item with three characters
      type read_chara2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(2)
      end type read_chara2_item
!
!>        structure of control item with three characters
      type read_chara3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(3)
      end type read_chara3_item
!
!>        structure of control item with three characters
      type read_chara_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_chara_real_item
!
!>        structure of control item with three characters
      type read_chara_real2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read real item
        real(kind = kreal) ::    realvalue(2)
      end type read_chara_real2_item
!
!>        structure of control item with three characters
      type read_chara2_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_chara2_real_item
!
!>        structure of control item with three characters
      type read_chara_int_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read integer items
        integer(kind = kint) ::  intvalue
      end type read_chara_int_item
!
!>        structure of control item with three characters
      type read_int_chara_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_int_chara_real_item
!
!>        structure of control item with three characters
      type read_int_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_int_real_item
!
!>        structure of control item with three characters
      type read_int2_real_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue
      end type read_int2_real_item
!
!>        structure of control item with three characters
      type read_int2_real2_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read integer items
        integer(kind = kint) ::  intvalue(2)
!>        array for read real item
        real(kind = kreal) ::    realvalue(2)
      end type read_int2_real2_item
!
!>        structure of control item with character and three integers
      type read_chara_int3_item
!>        read flag (If item is read iflag = 1)
        integer(kind = kint) ::  iflag = 0
!>        array for read character items
        character(len=kchara) ::  charavalue
!>        array for read integer items
        integer(kind = kint) ::  intvalue(3)
      end type read_chara_int3_item
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_real_ctl_type(c_buf, label, real_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real_item), intent(inout) :: real_item
!
      character(len=kchara) :: tmpchara
!
!
      if(real_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real_item%realvalue
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                       real_item%realvalue
      real_item%iflag = 1
!
      end subroutine read_real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_integer_ctl_type(c_buf, label, int_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_integer_item), intent(inout) :: int_item
!
      character(len=kchara) :: tmpchara
!
!
      if(int_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int_item%intvalue
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                        int_item%intvalue
      int_item%iflag = 1
!
       end subroutine read_integer_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_chara_ctl_type(c_buf, label, chara_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_character_item), intent(inout) :: chara_item
!
      character(len=kchara) :: tmpchara
!
!
      if(chara_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara_item%charavalue
      if (iflag_debug .gt. 0)  write(*,*) trim(c_buf%header_chara),     &
     &                       ': ', trim(chara_item%charavalue)
      chara_item%iflag = 1
!
       end subroutine read_chara_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_real2_ctl_type(c_buf, label, real2_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real2_item), intent(inout) :: real2_item
!
!
       character(len=kchara) :: tmpchara
!
!
      if(real2_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real2_item%realvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &        trim(c_buf%header_chara), ': ', real2_item%realvalue(1:2)
      real2_item%iflag = 1
!
       end subroutine read_real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_real3_ctl_type(c_buf, label, real3_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_real3_item), intent(inout) :: real3_item
!
       character(len=kchara) :: tmpchara
!
!
      if(real3_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, real3_item%realvalue(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,1p3e16.7)')               &
     &        trim(c_buf%header_chara), ': ', real3_item%realvalue(1:3)
      real3_item%iflag = 1
!
       end subroutine read_real3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_integer2_ctl_type(c_buf, label, int2_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_item), intent(inout) :: int2_item
!
      character(len=kchara) :: tmpchara
!
!
      if(int2_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, int2_item%intvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a2,2i6)')                    &
     &          trim(c_buf%header_chara), ': ', int2_item%intvalue(1:2)
      int2_item%iflag = 1
!
      end subroutine read_integer2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_character2_ctl_type(c_buf, label, chara2_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara2_item), intent(inout) :: chara2_item
!
       character(len=kchara) :: tmpchara
!
!
      if(chara2_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara2_item%charavalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 1: ', chara2_item%charavalue(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 2: ', chara2_item%charavalue(2)
      chara2_item%iflag = 1
!
      end subroutine read_character2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_character3_ctl_type(c_buf, label, chara3_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara3_item), intent(inout) :: chara3_item
!
       character(len=kchara) :: tmpchara
!
!
      if(chara3_item%iflag.gt.0                                         &
     &      .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, chara3_item%charavalue(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 1: ', chara3_item%charavalue(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 2: ', chara3_item%charavalue(2)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &      trim(c_buf%header_chara), ' 3: ', chara3_item%charavalue(3)
      chara3_item%iflag = 1
!
      end subroutine read_character3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_charreal2_ctl_type(c_buf, label, cr2_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_real2_item), intent(inout) :: cr2_item
!
       character(len=kchara) :: tmpchara
!
!
      if(cr2_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, cr2_item%charavalue,           &
     &                        cr2_item%realvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &            trim(c_buf%header_chara), ' cr2_item%charavalue: ',   &
     &            cr2_item%charavalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &     trim(c_buf%header_chara), ' real: ', cr2_item%realvalue(1:2)
      cr2_item%iflag = 1
!
      end subroutine read_charreal2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_char2real_ctl_type(c_buf, label, c2r_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara2_real_item), intent(inout) :: c2r_item
!
       character(len=kchara) :: tmpchara
!
!
      if(c2r_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, c2r_item%charavalue(1:2),      &
     &                         c2r_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &         trim(c_buf%header_chara), ' 1: ', c2r_item%charavalue(1)
      if (iflag_debug .gt. 0)  write(*,'(a,a4,a)')                      &
     &         trim(c_buf%header_chara), ' 2: ', c2r_item%charavalue(2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &         trim(c_buf%header_chara), ' real: ', c2r_item%realvalue
      c2r_item%iflag = 1
!
      end subroutine read_char2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_charareal_ctl_type(c_buf, label, cr_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_real_item), intent(inout) :: cr_item
!
       character(len=kchara) :: tmpchara
!
!
      if(cr_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, cr_item%charavalue,            &
     &                         cr_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &         trim(c_buf%header_chara), ' char: ', cr_item%charavalue
      if (iflag_debug .gt. 0)  write(*,'(a,a4,1pe23.15)')               &
     &         trim(c_buf%header_chara), ' vect: ', cr_item%realvalue
      cr_item%iflag = 1
!
      end subroutine read_charareal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_charaint_ctl_type(c_buf, label, ci_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_int_item), intent(inout) :: ci_item
!
       character(len=kchara) :: tmpchara
!
!
      if(ci_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ci_item%charavalue,            &
     &                        ci_item%intvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &          trim(c_buf%header_chara), ' char: ', ci_item%charavalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &          trim(c_buf%header_chara), ' int:  ', ci_item%intvalue
      ci_item%iflag = 1
!
      end subroutine read_charaint_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_intchrreal_ctl_type(c_buf, label, icr_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int_chara_real_item), intent(inout) :: icr_item
!
       character(len=kchara) :: tmpchara
!
!
      if(icr_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, icr_item%intvalue,             &
     &                         icr_item%charavalue, icr_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &         trim(c_buf%header_chara), ' int:  ', icr_item%intvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,a)')                      &
     &         trim(c_buf%header_chara), ' char: ', icr_item%charavalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &         trim(c_buf%header_chara), ' real: ', icr_item%realvalue
      icr_item%iflag = 1
!
      end subroutine read_intchrreal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_intreal_ctl_type(c_buf, label, ir_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int_real_item), intent(inout) :: ir_item
!
      character(len=kchara) :: tmpchara
!
!
      if(ir_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ir_item%intvalue,              &
     &                         ir_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,i16)')                    &
     &         trim(c_buf%header_chara), ' int:  ', ir_item%intvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &         trim(c_buf%header_chara), ' real: ', ir_item%realvalue
      ir_item%iflag = 1
!
      end subroutine read_intreal_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real_ctl_type(c_buf, label, i2r_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_real_item), intent(inout) :: i2r_item
!
       character(len=kchara) :: tmpchara
!
!
      if(i2r_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, i2r_item%intvalue(1:2),        &
     &                         i2r_item%realvalue
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &      trim(c_buf%header_chara), ' int:  ', i2r_item%intvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1pe23.15)')               &
     &      trim(c_buf%header_chara), ' real: ', i2r_item%realvalue
      i2r_item%iflag = 1
!
      end subroutine read_int2real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_int2real2_ctl_type(c_buf, label, i2r2_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_int2_real2_item), intent(inout) :: i2r2_item
!
       character(len=kchara) :: tmpchara
!
!
      if(i2r2_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, i2r2_item%intvalue(1:2),       &
     &                         i2r2_item%realvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &    trim(c_buf%header_chara), ' int:  ', i2r2_item%intvalue(1:2)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &    trim(c_buf%header_chara), ' real: ', i2r2_item%realvalue(1:2)
      i2r2_item%iflag = 1
!
      end subroutine read_int2real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine read_charaint3_ctl_type(c_buf, label, ci3_item)
!
      type(buffer_for_control), intent(in)  :: c_buf
      character(len=kchara), intent(in) :: label
      type(read_chara_int3_item), intent(inout) :: ci3_item
!
       character(len=kchara) :: tmpchara
!
!
      if(ci3_item%iflag.gt.0 .or. c_buf%header_chara.ne.label) return
!
      read(c_buf%ctl_buffer,*) tmpchara, ci3_item%charavalue,           &
     &                         ci3_item%intvalue(1:3)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,2i16)')                   &
     &       trim(c_buf%header_chara), ' ci3_item%charavalue:  ',       &
     &       trim(ci3_item%charavalue)
      if (iflag_debug .gt. 0)  write(*,'(a,a7,1p2e23.15)')              &
     &       trim(c_buf%header_chara), ' int: ', ci3_item%intvalue(1:3)
      ci3_item%iflag = 1
!
      end subroutine read_charaint3_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real_ctl_type                                    &
     &         (id_file, level, maxlen, label, real_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_real_item), intent(in) :: real_item
!
!
      if(real_item%iflag .eq. 0) return
      call write_real_ctl_item                                          &
     &   (id_file, level, maxlen, label, real_item%realvalue)
!
      end subroutine write_real_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_integer_ctl_type                                 &
     &         (id_file, level, maxlen, label, int_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_integer_item), intent(in) :: int_item
!
!
      if(int_item%iflag .eq. 0) return
      call write_integer_ctl_item                                       &
     &   (id_file, level, maxlen, label, int_item%intvalue)
!
       end subroutine write_integer_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_chara_ctl_type                                   &
     &         (id_file, level, maxlen, label, chara_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      integer(kind = kint), intent(in) :: maxlen
      character(len=kchara), intent(in) :: label
      type(read_character_item), intent(in) :: chara_item
!
!
      if(chara_item%iflag .eq. 0) return
      call write_character_ctl_item                                     &
     &   (id_file, level, maxlen, label, chara_item%charavalue)
!
       end subroutine write_chara_ctl_type
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_real2_ctl_type                                   &
     &         (id_file, level, label, real2_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_real2_item), intent(in) :: real2_item
!
!
      if(real2_item%iflag .eq. 0) return
      call write_real2_ctl_item(id_file, level, label,                  &
     &    real2_item%realvalue(1), real2_item%realvalue(2))
!
       end subroutine write_real2_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_real3_ctl_type                                   &
     &         (id_file, level, label, real3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_real3_item), intent(in) :: real3_item
!
!
      if(real3_item%iflag .eq. 0) return
      call write_real3_ctl_item(id_file, level, label,                  &
     &    real3_item%realvalue(1), real3_item%realvalue(2),             &
     &    real3_item%realvalue(3))
!
       end subroutine write_real3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_character3_ctl_type                              &
     &         (id_file, level, label, chara3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_chara3_item), intent(in) :: chara3_item
!
      integer(kind = kint) :: i
      integer(kind = kint) :: maxlen(0:2)
!
!
      if(chara3_item%iflag .eq. 0) return
!
      maxlen(0) = len_trim(label)
      do i = 1, 2
        maxlen(i) = len_trim(chara3_item%charavalue(i))                 &
     &           + iflag_divide(chara3_item%charavalue(i))
      end do
      call write_character3_ctl_item(id_file, level, label, maxlen,     &
     &    chara3_item%charavalue(1), chara3_item%charavalue(2),         &
     &    chara3_item%charavalue(3))
!
       end subroutine write_character3_ctl_type
!
!   --------------------------------------------------------------------
!
      subroutine write_charaint3_ctl_type                               &
     &         (id_file, level, label, ci3_item)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file, level
      character(len=kchara), intent(in) :: label
      type(read_chara_int3_item), intent(in) :: ci3_item
!
!
      if(ci3_item%iflag .eq. 0) return
!
      call write_chara_int3_ctl_item(id_file, level, label,             &
     &    ci3_item%charavalue, ci3_item%intvalue(1),                    &
     &    ci3_item%intvalue(2), ci3_item%intvalue(3))
!
       end subroutine write_charaint3_ctl_type
!
!   --------------------------------------------------------------------
!
      end module t_control_elements
