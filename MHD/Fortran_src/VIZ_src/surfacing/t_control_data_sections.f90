!
!      module t_control_data_sections
!
!      Written by H. Matsui on July, 2006
!
!      subroutine alloc_psf_ctl_stract(psf_ctls)
!
!      subroutine dealloc_psf_ctl_stract(psf_ctls)
!      subroutine dealloc_iso_ctl_stract(iso_ctls)
!
!!      subroutine read_sections_control_data                           &
!!     &         (id_control, psf_ctls, iso_ctls, c_buf)
!!
!!      subroutine read_files_4_psf_ctl                                 &
!!     &         (id_control, hd_block, psf_ctls, c_buf)
!!      subroutine read_files_4_iso_ctl                                 &
!!     &         (id_control, hd_block, iso_ctls, c_buf)
!!
!!      subroutine bcast_files_4_psf_ctl(psf_ctls)
!!      subroutine bcast_files_4_iso_ctl(iso_ctls)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array cross_section_ctl  1
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module t_control_data_sections
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_psf
      use t_control_data_4_iso
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
      type isosurf_controls
        integer(kind = kint) :: num_iso_ctl = 0
        character(len = kchara), allocatable :: fname_iso_ctl(:)
        type(iso_ctl), allocatable :: iso_ctl_struct(:)
      end type isosurf_controls
!
!   entry label
!
      character(len=kchara), parameter :: hd_viz_ctl = 'visual_control'
      integer (kind=kint) :: i_viz_ctl = 0
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
!
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
!
      private :: hd_section_ctl, hd_psf_ctl
      private :: hd_isosurf_ctl, hd_iso_ctl
      private :: hd_viz_ctl, i_viz_ctl
!
      private :: alloc_iso_ctl_stract
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
!
      subroutine alloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      allocate(iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl))
      allocate(iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl))
!
      end subroutine alloc_iso_ctl_stract
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
!
      subroutine dealloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      deallocate(iso_ctls%iso_ctl_struct)
      deallocate(iso_ctls%fname_iso_ctl)
!
      end subroutine dealloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_sections_control_data                             &
     &         (id_control, psf_ctls, iso_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(section_controls), intent(inout) :: psf_ctls
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_viz_ctl) .eqv. .FALSE.) return
!
      if(i_viz_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_viz_ctl)) exit
!
        if(check_array_flag(c_buf, hd_psf_ctl)) then
          call read_files_4_psf_ctl                                     &
     &       (id_control, hd_psf_ctl, psf_ctls, c_buf)
        end if
        if(check_array_flag(c_buf, hd_section_ctl)) then
          call read_files_4_psf_ctl                                     &
     &     (id_control, hd_section_ctl, psf_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_iso_ctl)) then
          call read_files_4_iso_ctl                                     &
     &       (id_control, hd_iso_ctl, iso_ctls, c_buf)
        end if
        if(check_array_flag(c_buf, hd_isosurf_ctl)) then
          call read_files_4_iso_ctl                                     &
     &       (id_control, hd_isosurf_ctl, iso_ctls, c_buf)
        end if
      end do
      i_viz_ctl = 1
!
      end subroutine read_sections_control_data
!
!   --------------------------------------------------------------------
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
!
      subroutine read_files_4_iso_ctl                                   &
     &         (id_control, hd_block, iso_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: i
!
!
      if(allocated(iso_ctls%fname_iso_ctl)) return
      iso_ctls%num_iso_ctl = 0
      call alloc_iso_ctl_stract(iso_ctls)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)) then
          call append_new_isosurface_control(iso_ctls)
          iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl)                  &
     &        = third_word(c_buf)
        else if(check_begin_flag(c_buf, hd_block)) then
          call append_new_isosurface_control(iso_ctls)
          iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl) = 'NO_FILE'
          call read_iso_control_data(id_control, hd_block,              &
     &        iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl), c_buf)
        end if
      end do
!
      write(*,*) 'iso_ctls%num_iso_ctl', iso_ctls%num_iso_ctl
      do i= 1, iso_ctls%num_iso_ctl
        write(*,*) i, iso_ctls%fname_iso_ctl(i)
      end do
!
      end subroutine read_files_4_iso_ctl
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
!
      subroutine bcast_files_4_iso_ctl(iso_ctls)
!
      use calypso_mpi
      use t_control_data_4_iso
!
      type(isosurf_controls), intent(inout) :: iso_ctls
      integer (kind=kint) :: i_iso
!
!
      call MPI_BCAST(iso_ctls%num_iso_ctl,  1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(iso_ctls%num_iso_ctl .le. 0) return
!
      if(my_rank .gt. 0) call alloc_iso_ctl_stract(iso_ctls)
!
      call MPI_BCAST                                                    &
     &   (iso_ctls%fname_iso_ctl, int(kchara*iso_ctls%num_iso_ctl),     &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
      do i_iso = 1, iso_ctls%num_iso_ctl
        if(iso_ctls%fname_iso_ctl(i_iso) .eq. 'NO_FILE') then
          call bcast_iso_control_data(iso_ctls%iso_ctl_struct(i_iso))
        end if
      end do
!
      end subroutine bcast_files_4_iso_ctl
!
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
        call deallocate_cont_dat_4_psf(psf_c(i))
      end do
!
      end subroutine dealloc_cont_dat_4_psfs
!
!  ---------------------------------------------------------------------
!
      subroutine append_new_isosurface_control(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      type(isosurf_controls) :: tmp_iso_c
!
!
      tmp_iso_c%num_iso_ctl = iso_ctls%num_iso_ctl
      call alloc_iso_ctl_stract(tmp_iso_c)
      call dup_control_4_isos                                           &
     &    (tmp_iso_c%num_iso_ctl, iso_ctls, tmp_iso_c)
!
      call dealloc_cont_dat_4_isos                                      &
     &   (iso_ctls%num_iso_ctl, iso_ctls%iso_ctl_struct)
      call dealloc_iso_ctl_stract(iso_ctls)
!
      iso_ctls%num_iso_ctl = tmp_iso_c%num_iso_ctl + 1
      call alloc_iso_ctl_stract(iso_ctls)
!
      call dup_control_4_isos                                           &
     &   (tmp_iso_c%num_iso_ctl, tmp_iso_c, iso_ctls)
!
      call dealloc_cont_dat_4_isos                                      &
     &   (tmp_iso_c%num_iso_ctl, tmp_iso_c%iso_ctl_struct)
      call dealloc_iso_ctl_stract(tmp_iso_c)
!
      end subroutine append_new_isosurface_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_isos                                     &
     &         (num_iso, org_iso_ctls, new_iso_ctls)
!
      integer(kind = kint), intent(in) :: num_iso
      type(isosurf_controls), intent(in) :: org_iso_ctls
      type(isosurf_controls), intent(inout) :: new_iso_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        if(org_iso_ctls%fname_iso_ctl(i) .eq. 'NO_FILE') then
          call dup_control_4_iso(org_iso_ctls%iso_ctl_struct(i),        &
              new_iso_ctls%iso_ctl_struct(i))
        end if
        new_iso_ctls%fname_iso_ctl(i) = org_iso_ctls%fname_iso_ctl(i)
      end do
!
      end subroutine dup_control_4_isos
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_isos(num_iso, iso_c)
!
      integer(kind = kint), intent(in) :: num_iso
      type(iso_ctl), intent(inout) :: iso_c(num_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        call deallocate_cont_dat_4_iso(iso_c(i))
      end do
!
      end subroutine dealloc_cont_dat_4_isos
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_sections
