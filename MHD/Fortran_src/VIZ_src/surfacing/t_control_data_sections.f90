!>@file   t_control_data_sections.f90
!!@brief  module t_control_data_sections
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine alloc_psf_ctl_stract(psf_ctls)
!!      subroutine dealloc_psf_ctl_stract(psf_ctls)
!!
!!      subroutine read_files_4_psf_ctl                                 &
!!     &         (id_control, hd_block, psf_ctls, c_buf)
!!      subroutine bcast_files_4_psf_ctl(psf_ctls)
!!        type(section_controls), intent(inout) :: psf_ctls
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array cross_section_ctl  1
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_sections
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_psf
!
      implicit  none
!
!
      type section_controls
        integer(kind = kint) :: num_psf_ctl = 0
        character(len = kchara), allocatable :: fname_psf_ctl(:)
        type(psf_ctl), allocatable :: psf_ctl_struct(:)
      end type section_controls
!
      private :: append_new_section_control
      private :: dup_control_4_psfs, dealloc_cont_dat_4_psfs
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_ctl_stract(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
      integer(kind = kint) :: i
!
!
      allocate(psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl))
      allocate(psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl))
!
      do i = 1, psf_ctls%num_psf_ctl
        call init_psf_ctl_stract(psf_ctls%psf_ctl_struct(i))
      end do
!
      end subroutine alloc_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_ctl_stract(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
!
      deallocate(psf_ctls%psf_ctl_struct)
      deallocate(psf_ctls%fname_psf_ctl)
!
      end subroutine dealloc_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_files_4_psf_ctl                                   &
     &         (id_control, hd_block, psf_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(section_controls), intent(inout) :: psf_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: i
!
!
      if(allocated(psf_ctls%fname_psf_ctl)) return
      psf_ctls%num_psf_ctl = 0
      call alloc_psf_ctl_stract(psf_ctls)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)) then
          call append_new_section_control(psf_ctls)
          psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl)                  &
     &        = third_word(c_buf)
        else if(check_begin_flag(c_buf, hd_block)) then
          call append_new_section_control(psf_ctls)
          psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl) = 'NO_FILE'
          call read_psf_control_data(id_control, hd_block,              &
     &        psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl), c_buf)
        end if
      end do
!
      write(*,*) 'psf_ctls%num_psf_ctl', psf_ctls%num_psf_ctl
      do i= 1, psf_ctls%num_psf_ctl
        write(*,*) i, psf_ctls%fname_psf_ctl(i)
      end do
!
      end subroutine read_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_files_4_psf_ctl(psf_ctls)
!
      use calypso_mpi
      use t_control_data_4_psf
!
      type(section_controls), intent(inout) :: psf_ctls
      integer (kind=kint) :: i_psf
!
!
      call MPI_BCAST(psf_ctls%num_psf_ctl, 1,                           &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(psf_ctls%num_psf_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_psf_ctl_stract(psf_ctls)
!
      call MPI_BCAST                                                    &
     &   (psf_ctls%fname_psf_ctl, int(kchara*psf_ctls%num_psf_ctl),     &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      do i_psf = 1, psf_ctls%num_psf_ctl
        if(psf_ctls%fname_psf_ctl(i_psf) .eq. 'NO_FILE') then
          call bcast_psf_control_data(psf_ctls%psf_ctl_struct(i_psf))
        end if
      end do
!
      end subroutine bcast_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_new_section_control(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
!
      type(section_controls) :: tmp_psf_c
!
!
      tmp_psf_c%num_psf_ctl = psf_ctls%num_psf_ctl
      call alloc_psf_ctl_stract(tmp_psf_c)
      call dup_control_4_psfs                                           &
     &    (tmp_psf_c%num_psf_ctl, psf_ctls, tmp_psf_c)
!
      call dealloc_cont_dat_4_psfs                                      &
     &   (psf_ctls%num_psf_ctl, psf_ctls%psf_ctl_struct)
      call dealloc_psf_ctl_stract(psf_ctls)
!
      psf_ctls%num_psf_ctl = tmp_psf_c%num_psf_ctl + 1
      call alloc_psf_ctl_stract(psf_ctls)
!
      call dup_control_4_psfs                                           &
     &   (tmp_psf_c%num_psf_ctl, tmp_psf_c, psf_ctls)
!
      call dealloc_cont_dat_4_psfs                                      &
     &   (tmp_psf_c%num_psf_ctl, tmp_psf_c%psf_ctl_struct)
      call dealloc_psf_ctl_stract(tmp_psf_c)
!
      end subroutine append_new_section_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_psfs                                     &
     &         (num_psf, org_psf_ctls, new_psf_ctls)
!
      integer(kind = kint), intent(in) :: num_psf
      type(section_controls), intent(in) :: org_psf_ctls
      type(section_controls), intent(inout) :: new_psf_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        if(org_psf_ctls%fname_psf_ctl(i) .eq. 'NO_FILE') then
          call dup_control_4_psf(org_psf_ctls%psf_ctl_struct(i),        &
              new_psf_ctls%psf_ctl_struct(i))
        end if
        new_psf_ctls%fname_psf_ctl(i) = org_psf_ctls%fname_psf_ctl(i)
      end do
!
      end subroutine dup_control_4_psfs
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_psfs(num_psf, psf_c)
!
      integer(kind = kint), intent(in) :: num_psf
      type(psf_ctl), intent(inout) :: psf_c(num_psf)
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dealloc_cont_dat_4_psf(psf_c(i))
      end do
!
      end subroutine dealloc_cont_dat_4_psfs
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_sections
