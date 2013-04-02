!
!      module cal_psf_rms_aves
!
      module cal_psf_rms_aves
!
!      Written by H. Matsui on Apr., 2005
!      Modified by H. Matsui on Feb., 2009
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: nstep_old, nstep_total
      integer(kind = kint), allocatable :: istep(:)
      real(kind = kreal), allocatable :: ave_psf_evo(:,:,:)
      real(kind = kreal), allocatable :: rms_psf_evo(:,:,:)
      real(kind = kreal), allocatable :: area_total_psf_evo(:,:)
!
      integer(kind = kint), parameter :: id_ave_psf = 21
      integer(kind = kint), parameter :: id_rms_psf = 22
      character(len=kchara), parameter :: fname_ave_psf = 'psf_ave.dat'
      character(len=kchara), parameter :: fname_rms_psf = 'psf_rms.dat'
!
      character(len=kchara) :: fname_psf, tmpchara
      integer(kind = kint), parameter :: izero = 0, ione = 1
!
      private :: nstep_old, nstep_total
      private :: istep, ave_psf_evo, rms_psf_evo, area_total_psf_evo
      private :: id_ave_psf, fname_ave_psf
      private :: id_rms_psf, fname_rms_psf, fname_psf, tmpchara
      private :: izero, ione
!
!      subroutine open_psf_ave_rms_data
!      subroutine open_new_psf_ave_rms_data
!      subroutine s_cal_psf_rms_aves
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_psf_ave_rms_evo
!
      use m_control_params_4_psf
      use m_psf_results
      use m_t_step_parameter
      use set_control_visualizer
!
      nstep_total = nstep_old + 1                                       &
     &             + (i_step_number-i_step_init) / i_step_output_psf
      allocate( ave_psf_evo(ncomptot_psf,nstep_total,num_psf) )
      allocate( rms_psf_evo(ncomptot_psf,nstep_total,num_psf) )
      allocate( area_total_psf_evo(nstep_total,num_psf) )
      allocate( istep(nstep_total) )
      ave_psf_evo = 0.0d0
      rms_psf_evo = 0.0d0
      area_total_psf_evo = 0.0d0
      istep = 0
!
      end subroutine allocate_psf_ave_rms_evo
!
!-----------------------------------------------------------------------
!
      subroutine open_psf_ave_rms_data
!
      use m_control_params_4_psf
      use m_psf_results
      use m_t_step_parameter
      use m_field_file_format
!
      use set_control_visualizer
      use read_psf_result
      use take_normals_4_psf
      use set_ucd_file_names
!
      integer(kind = kint) :: i, i_psf, ierr, icou_a, icou_r, itmp
!
!
!   read psf data for initial step
!
      call set_single_ucd_file_name(psf_header(ione), iflag_udt,        &
     &    i_step_init, fname_psf)
!
      write(*,*) 'PSF result data: ', fname_psf
      open(id_psf_result, file=fname_psf, form='formatted')
!
      numnod_psf = 0
      numele_psf = 0
      call allocate_psf_results
!
      call read_allocate_psf_ncomps_udt
      call read_psf_field_name
      close(id_psf_result)
!
!
!    open old psf averaging data
!
      write(*,*) 'average file name: psf_ave.dat'
      write(*,*) 'RMS file name:     psf_rms.dat'
!
      open(id_ave_psf, file=fname_ave_psf, form='formatted',            &
     &     status='old',err = 100)
      open(id_rms_psf, file=fname_rms_psf, form='formatted',            &
     &     status='old',err = 100)
!
!     check headers
!
      read(id_ave_psf,*,end=110,err=110) tmpchara
      call check_headers_psf_comp_name(id_ave_psf, ierr)
      if(ierr .gt. 0) go to 110
      read(id_ave_psf,*,end=110,err=110) tmpchara
!
      read(id_rms_psf,*,end=110,err=110) tmpchara
      call check_headers_psf_comp_name(id_rms_psf, ierr)
      if(ierr .gt. 0) go to 110
!
      icou_a = 0
      do
        read(id_ave_psf,*,end=200,err=200) itmp, i,                     &
     &      ave_psf(1:ncomptot_psf), area_total_psf
        icou_a = icou_a + 1
      end do
  200 continue
!
      icou_r = 0
      do
        read(id_rms_psf,*,end=300,err=300) itmp, i,                     &
     &      rms_psf(1:ncomptot_psf)
        icou_r = icou_r + 1
      end do
  300 continue
!
      close(id_ave_psf)
      close(id_rms_psf)
!
      write(*,*) 'icou', icou_r, icou_a, num_psf
      if (icou_r .ne. icou_a) go to 110
      if (mod(icou_r,num_psf) .ne. 0) go to 110
!
      nstep_old = icou_r / num_psf
      write(*,*) 'nstep_old', nstep_old
      call allocate_psf_ave_rms_evo
!
!     read old data
!
      open(id_ave_psf, file=fname_ave_psf, form='formatted',            &
     &     status='old',err = 100)
      open(id_rms_psf, file=fname_rms_psf, form='formatted',            &
     &     status='old',err = 100)
!
      read(id_ave_psf,*,end=110,err=110) tmpchara
      call check_headers_psf_comp_name(id_ave_psf, ierr)
      read(id_ave_psf,*,end=110,err=110) tmpchara
!
      read(id_rms_psf,*,end=110,err=110) tmpchara
      call check_headers_psf_comp_name(id_rms_psf, ierr)
!
      do i_psf = 1, num_psf
        do i = 1, nstep_old
          read(id_ave_psf,*) itmp, istep(i),                            &
     &      ave_psf_evo(1:ncomptot_psf,i,i_psf),                        &
     &      area_total_psf_evo(i,i_psf)
          read(id_rms_psf,*) itmp, istep(i),                            &
     &      rms_psf_evo(1:ncomptot_psf,i,i_psf)
        end do
      end do
!
      close(id_ave_psf)
      close(id_rms_psf)
!
      call deallocate_psf_results
!
      return
!
!  construct new rms and average file
!
  100 continue
      call deallocate_psf_results
      nstep_old = 0
      call allocate_psf_ave_rms_evo
      return
!
!   error message and stop
!
  110 continue
      write(*,*) 'move old PSF averaged data files'
      stop
!
      end subroutine open_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine open_new_psf_ave_rms_data
!
      use m_control_params_4_psf
      use m_psf_results
      use m_t_step_parameter
      use m_field_file_format
!
      use set_control_visualizer
      use read_psf_result
      use take_normals_4_psf
      use set_ucd_file_names
!
!
      open(id_ave_psf, file=fname_ave_psf, form='formatted')
      open(id_rms_psf, file=fname_rms_psf, form='formatted')
!
!
!   read psf data for initial step
!
      call set_single_ucd_file_name(psf_header(ione), iflag_udt,        &
     &    i_step_init, fname_psf)
!
      write(*,*) 'PSF result data: ', fname_psf
      open(id_psf_result, file=fname_psf, form='formatted')
!
      numnod_psf = 0
      numele_psf = 0
      call allocate_psf_results
!
      call read_allocate_psf_ncomps_udt
      call read_psf_field_name
      close(id_psf_result)
!
!
      write(id_ave_psf,'(a)') ' psf_no, step_no, '
      call write_headers_psf_comp_name(id_ave_psf)
      write(id_ave_psf,*) 'area_size, '
!
      write(id_rms_psf,'(a)') ' psf_no, step_no, '
      call write_headers_psf_comp_name(id_rms_psf)
!
      call deallocate_psf_results
!
      end subroutine open_new_psf_ave_rms_data
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_psf_rms_aves
!
      use m_control_params_4_psf
      use m_psf_results
      use m_t_step_parameter
      use m_field_file_format
!
      use set_control_visualizer
      use read_psf_result
      use read_psf_select_4_zlib
      use take_normals_4_psf
      use take_avarages_4_psf
      use set_ucd_file_names
!
      integer(kind = kint) :: i, i_psf, icou
!
!
      do i_psf = 1, num_psf
        call sel_read_psf_grid_file(iflag_udt, psf_header(i_psf) )
!
        call allocate_norms_4_psf
        call cal_norm_area_4_psf
!
!
        call set_single_ucd_file_name(psf_header(i_psf), iflag_udt,     &
     &      i_step_init, fname_psf)
!
        write(*,*) 'PSF udt data: ', fname_psf
        open(id_psf_result, file=fname_psf, form='formatted')
        call read_allocate_psf_ncomps_udt
        close(id_psf_result)
!
!
        icou = nstep_old
        do i = i_step_init, i_step_number, i_step_output_psf
          call sel_read_psf_udt_file(iflag_udt, psf_header(i_psf), i)
!
          call cal_rms_ave_4_psf
!
          icou = icou + 1
          ave_psf_evo(1:ncomptot_psf,icou,i_psf)                        &
     &         = ave_psf(1:ncomptot_psf)
          rms_psf_evo(1:ncomptot_psf,icou,i_psf)                        &
     &         = rms_psf(1:ncomptot_psf)
          area_total_psf_evo(icou,i_psf) = area_total_psf
          istep(icou) = i
!
        end do
!
        call deallocate_norms_4_psf
        call deallocate_psf_results
      end do
!
!      write results
!
      do i_psf = 1, num_psf
        do i = 1, nstep_total
          write(id_ave_psf,'(2i10,1p255E25.15e3)') i_psf, istep(i),     &
     &           ave_psf_evo(1:ncomptot_psf,i,i_psf),                   &
     &           area_total_psf_evo(i,i_psf)
          write(id_rms_psf,'(2i10,1p255E25.15e3)') i_psf, istep(i),     &
     &           rms_psf_evo(1:ncomptot_psf,i,i_psf)
        end do
      end do
!
      close(id_ave_psf)
      close(id_rms_psf)
!
      end subroutine s_cal_psf_rms_aves
!
!-----------------------------------------------------------------------
!
      end module cal_psf_rms_aves
