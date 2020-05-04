!>@file   sel_read_rayleigh_restart.f90
!!@brief  module sel_read_rayleigh_restart
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine sel_read_rayleigh_rst_params(dir, i_step, ra_rst)
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!
!!      subroutine set_rayleigh_rst_file_name(i_version, dir, i_step,   &
!!     &          field_name, iflag_ncomp, file_name)
!!      character(len = kchara) function sel_rayleigh_file_name         &
!!     &                               (i_version, dir, int_id, postfix)
!!          Version 0.99: "[dir]/[int_id]_[field_flag]"
!!          Version 1.x:  "[dir]/[int_id]/[field_flag]"
!!@endverbatim
!
      module sel_read_rayleigh_restart
!
      use m_precision
      use t_rayleigh_restart_IO
!
      implicit  none
!
      private :: read_rayleigh99_restart_params
      private :: set_rayleigh99_file_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_rayleigh_rst_params(dir, i_step, ra_rst)
!
      integer(kind = kint), intent(in) :: i_step
      character(len = kchara), intent(in) :: dir
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
!
      if(ra_rst%i_version .lt. 1) then
        call read_rayleigh99_restart_params(dir, i_step, ra_rst)
      else
        call read_rayleigh_restart_params(dir, i_step, ra_rst)
      end if
!
      end subroutine sel_read_rayleigh_rst_params
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function sel_rayleigh_file_name           &
     &                               (i_version, dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: i_version, int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      if(i_version .lt. 1) then
        sel_rayleigh_file_name                                          &
     &      = set_rayleigh99_file_name(dir, int_id, postfix)
      else
        sel_rayleigh_file_name                                          &
     &      = set_rayleigh_file_name(dir, int_id, postfix)
      end if
!
      end function sel_rayleigh_file_name
!
!-----------------------------------------------------------------------
!
      subroutine set_rayleigh_rst_file_name(i_version, dir, i_step,     &
     &          field_name, iflag_ncomp, file_name)
!
      use t_base_field_labels
      use t_explicit_term_labels
!
      integer(kind = kint), intent(in) :: i_version, i_step
      character(len = kchara), intent(in) :: dir
      character(len = kchara), intent(in) :: field_name
!
      integer(kind = kint), intent(inout) :: iflag_ncomp
      character(len = kchara), intent(inout) :: file_name(2)
!
      if(field_name .eq. velocity%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         wchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         zchar)
      else if(field_name .eq. pressure%name) then
        iflag_ncomp = 1
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         pchar)
      else if(field_name .eq. temperature%name) then
        iflag_ncomp = 1
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         tchar)
      else if(field_name .eq. magnetic_field%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         cchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         achar)
!
      else if(field_name .eq. previous_momentum%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         wabchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         zabchar)
!      else if(field_name .eq. previous_pressure%name) then
!        iflag_ncomp = 1
!        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step, &
!     &                                         cchar)
      else if(field_name .eq. previous_heat%name) then
        iflag_ncomp = 1
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         tabchar)
      else if(field_name .eq. previous_induction%name) then
        iflag_ncomp = 2
        file_name(1) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         cabchar)
        file_name(2) =  sel_rayleigh_file_name(i_version, dir, i_step,  &
     &                                         aabchar)
      end if
!
      end subroutine set_rayleigh_rst_file_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh99_restart_params(dir, i_step, ra_rst)
!
      integer(kind = kint), intent(in) :: i_step
      character(len = kchara), intent(in) :: dir
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      integer, parameter :: id_file = 15
      character(len = kchara) :: file_name
      integer :: i4_tmp
      integer(kind = kint_gl) :: l8_byte
!
!
      write(*,*) 'i_step', i_step
      file_name =  set_rayleigh99_file_name(dir, i_step, paramchar)
      write(*,*) 'read Rayleigh checkpoint paramter file: ',            &
     &          trim(file_name)
        open(id_file, FILE=file_name, STATUS='OLD',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
!
      ra_rst%iflag_swap = iendian_KEEP
      read(id_file) i4_tmp
      if(i4_tmp .ne. 4) ra_rst%iflag_swap = iendian_FLIP
!
      read(id_file) ra_rst%nri_org, i4_tmp
      read(id_file) i4_tmp, ra_rst%iflag_rtype, i4_tmp
      read(id_file) i4_tmp, ra_rst%ltr_org, i4_tmp
!
      read(id_file) i4_tmp, ra_rst%dt_org, i4_tmp
      read(id_file) i4_tmp, ra_rst%dt_new, i4_tmp
!      read(id_file) i4_tmp, ra_rst%new_dt_org, i4_tmp
!
      if(ra_rst%iflag_swap .eq. iendian_FLIP) then
        l8_byte = kint_4b
        call byte_swap_32bit_f                                          &
     &     (l8_byte, ra_rst%i_version_from_file)
        call byte_swap_32bit_f(l8_byte, ra_rst%nri_org)
        call byte_swap_32bit_f(l8_byte, ra_rst%iflag_rtype)
        call byte_swap_32bit_f(l8_byte, ra_rst%ltr_org)
        l8_byte = kreal
        call byte_swap_64bit_f(l8_byte, ra_rst%dt_org)
        call byte_swap_64bit_f(l8_byte, ra_rst%dt_new)
 !       call byte_swap_64bit_f(l8_byte, ra_rst%new_dt_org)
      end if
!
      call alloc_rayleigh_radial_grid(ra_rst)
!
      read(id_file) i4_tmp, ra_rst%r_org(1:ra_rst%nri_org), i4_tmp
      read(id_file) i4_tmp, ra_rst%time_org, i4_tmp
!
      close(id_file)
!
      if(ra_rst%iflag_swap .eq. iendian_FLIP) then
        l8_byte = ra_rst%nri_org * kreal
        call byte_swap_64bit_f(l8_byte, ra_rst%r_org)
        l8_byte = kreal
        call byte_swap_64bit_f(l8_byte, ra_rst%time_org)
      end if
!
      end subroutine read_rayleigh99_restart_params
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function set_rayleigh99_file_name         &
     &                               (dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      write(set_rayleigh99_file_name,1000)                              &
     &                        trim(dir), int_id, trim(postfix)
 1000 format(a, '/', i8.8, '_', a)
!
      end function set_rayleigh99_file_name
!
!-----------------------------------------------------------------------
!
      end module sel_read_rayleigh_restart
