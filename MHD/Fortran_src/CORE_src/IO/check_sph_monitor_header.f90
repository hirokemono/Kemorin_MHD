!>@file   cal_write_sph_monitor_data.f90
!!@brief  module cal_write_sph_monitor_data
!!
!!@author H. Matsui
!!@date Programmed in 2009
!
!>@brief  I/O routines for mean square and averaga data
!!
!!@verbatim
!!      logical function error_sph_moniter_two_int(id_file, label,      &
!!     &                                           int_ref1, int_ref2)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: label(2)
!!        integer(kind = kint), intent(in) :: int_ref1, int_ref2
!!      logical function error_sph_moniter_int_real(id_file, label,     &
!!     &                                            int_ref, real_ref)
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len=kchara), intent(in) :: label(2)
!!        integer(kind = kint), intent(in) :: int_ref
!!        real(kind = kreal), intent(in) :: real_ref
!!      logical function error_sph_moniter_ncomp(id_file, num_fld,      &
!!     &                                         ncomp_monitor, pwr_name)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: num_fld
!!        integer(kind = kint), intent(in) :: ncomp_monitor(num_fld)
!!        character(len=kchara), intent(in) :: pwr_name(num_fld)
!!      logical function error_sph_moniter_items(id_file, mode_label,   &
!!     &                                         ntot_comp, item_name)
!!        type(energy_label_param), intent(in) :: ene_labels
!!        integer(kind = kint), intent(in) :: id_file
!!        character(len = kchara), intent(in) :: mode_label
!!        integer(kind = kint), intent(in) :: ntot_comp
!!        character(len=kchara), intent(in) :: item_name(ntot_comp)
!!@endverbatim
      module check_sph_monitor_header
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_two_int(id_file, label,        &
     &                                           int_ref1, int_ref2)
!
      integer(kind = kint), intent(in) :: id_file
!
      character(len=kchara), intent(in) :: label(2)
      integer(kind = kint), intent(in) :: int_ref1, int_ref2
!
      integer(kind = kint) :: int_read1, int_read2
!
!
      read(id_file,*) int_read1, int_read2
!
      error_sph_moniter_two_int = .FALSE.
      if(int_read1 .ne. int_ref1) then
        write(*,*) trim(label(1)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_two_int = .TRUE.
        return
      end if
!
      if(int_read2 .ne. int_ref2) then
        write(*,*) trim(label(2)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_two_int = .TRUE.
        return
      end if
!
      end function error_sph_moniter_two_int
!
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_int_real(id_file, label,       &
     &                                            int_ref, real_ref)
!
      integer(kind = kint), intent(in) :: id_file
!
      character(len=kchara), intent(in) :: label(2)
      integer(kind = kint), intent(in) :: int_ref
      real(kind = kreal), intent(in) :: real_ref
!
      integer(kind = kint) :: int_read
      real(kind = kreal) :: real_read
!
!
      read(id_file,*) int_read, real_read
!
      error_sph_moniter_int_real = .FALSE.
      if(int_read .ne. int_ref) then
        write(*,*) trim(label(1)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_int_real = .TRUE.
        return
      end if
!
      if(real(real_read) .ne. real(real_ref)) then
        write(*,*) trim(label(2)),                                      &
     &             ' does not match between data and file'
        error_sph_moniter_int_real = .TRUE.
        return
      end if
!
      end function error_sph_moniter_int_real
!
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_ncomp(id_file, num_fld,        &
     &                                         ncomp_monitor, pwr_name)
!
      type(energy_label_param), intent(in) :: ene_labels
      integer(kind = kint), intent(in) :: id_file
!
      integer(kind = kint), intent(in) :: num_fld
      integer(kind = kint), intent(in) :: ncomp_monitor(num_fld)
      character(len=kchara), intent(in) :: pwr_name(num_fld)
!
      integer(kind = kint), allocatable :: num_read_comp(:)
!
      integer(kind = kint) :: icou
!
!
      allocate(num_read_comp(num_fld))
!
      read(id_file,*) num_read_comp(1:num_fld)
!
      do icou = 1, num_fld
        if(num_read_comp(icou) .ne. ncomp_monitor(icou)) then
          write(*,*) 'Number of component for ', trim(pwr_name(icou)),  &
     &               ' does not match with the data file'
          error_sph_moniter_ncomp = .TRUE.
          deallocate(num_read_comp)
          return
        end if
      end do
!
      deallocate(num_read_comp)
      error_sph_moniter_ncomp = .FALSE.
!
      end function error_sph_moniter_ncomp
!
!  --------------------------------------------------------------------
!
      logical function error_sph_moniter_items(id_file, mode_label,     &
     &                                         ntot_comp, item_name)
!
      use skip_comment_f
!
      type(energy_label_param), intent(in) :: ene_labels
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
!
      integer(kind = kint), intent(in) :: ntot_comp
      character(len=kchara), intent(in) :: item_name(ntot_comp)
!
      character (len=kchara), allocatable :: read_name(:)
!
      character(len=kchara) :: tmpc1, tmpc2, tmpc3
      integer(kind = kint) :: icou
!
!
      allocate(read_name(ntot_comp))
!
      if(mode_label .ne. 'EMPTY') then
        read(id_file,*) tmpc1, tmpc2, tmpc3, read_name(1:ntot_comp)
      else
        read(id_file,*) tmpc1, tmpc2, read_name(1:ntot_comp)
      end if
!
      do icou = 1, ntot_comp
        if(cmp_no_case(read_name(icou), item_name(icou))                &
     &                                         .eqv. .FALSE.) then
          write(*,*) 'field ', trim(item_name(icou)),                   &
     &                 ' does not match with the data file',            &
     &                 read_name(icou), item_name(icou)
          error_sph_moniter_items = .TRUE.
          deallocate(read_name)
          return
        end if
      end do
!
      deallocate(read_name)
      error_sph_moniter_items = .FALSE.
!
      end function error_sph_moniter_items
!
!  --------------------------------------------------------------------
!
      end module check_sph_monitor_header
