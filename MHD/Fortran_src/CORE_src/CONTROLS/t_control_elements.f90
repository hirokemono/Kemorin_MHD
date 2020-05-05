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
      end module t_control_elements
