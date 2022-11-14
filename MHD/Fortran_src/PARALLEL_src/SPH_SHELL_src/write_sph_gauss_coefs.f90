!>@file   write_sph_gauss_coefs.f90
!!@brief  module write_sph_gauss_coefs
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_sph_gauss_coefs_file                          &
!!     &         (time_d, sph_params, sph_rj, ipol, rj_fld,             &
!!     &          gauss, SR_sig)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: gauss
!!        type(send_recv_status), intent(inout) :: SR_sig
!!
!!     logical function error_gauss_coefs_header(sph_params, sph_rj,    &
!!    &                                          gauss)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(picked_spectrum_data), intent(in) :: gauss
!!@endverbatim
!!
      module write_sph_gauss_coefs
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_address
      use t_phys_data
      use t_time_data
      use m_monitor_file_labels
!
      implicit  none
!
      integer(kind = kint), parameter, private :: id_gauss_coef = 23
!
      private :: write_sph_gauss_coefes, picked_gauss_head
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_sph_gauss_coefs_file                            &
     &         (time_d, sph_params, sph_rj, ipol, rj_fld,               &
     &          gauss, SR_sig)
!
      use t_solver_SR
      use set_parallel_file_name
      use delete_data_files
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
!
      type(send_recv_status), intent(inout) :: SR_sig
!
      real(kind=kreal), allocatable :: d_rj_out(:)
!
      character(len=kchara) :: file_name
      character(len=kchara) :: fmt_txt
!
!
      if(gauss%num_sph_mode .le. 0) return
!
      if(my_rank .eq. 0) then
        allocate(d_rj_out(gauss%istack_picked_spec_lc(nprocs)))
!$omp parallel workshare
        d_rj_out(1:gauss%istack_picked_spec_lc(nprocs)) = 0.0d0
!$omp end parallel workshare
      else
        allocate(d_rj_out(0))
      end if
!
      call write_sph_gauss_coefes(sph_params, sph_rj, ipol, rj_fld,     &
     &    gauss, gauss%istack_picked_spec_lc(nprocs), d_rj_out, SR_sig)
!
      if(my_rank .eq. 0) then
        file_name = add_dat_extension(gauss%file_prefix)
        if(check_file_exist(file_name)) then 
          open(id_gauss_coef,file=file_name, status='old',              &
      &        form='formatted',position='append')
        else
          open(id_gauss_coef,file=file_name, status='new',              &
     &         form='formatted')
          call write_sph_gauss_coefs_header(id_gauss_coef,              &
     &        sph_params%l_truncation, sph_rj%nidx_rj(1),               &
     &        sph_params%nlayer_ICB, sph_params%nlayer_CMB, gauss)
        end if
!
        write(fmt_txt,'(a1,i8,a13)')                                    &
     &      '(', gauss%istack_picked_spec_lc(nprocs), '(1pE25.14e3))'
!
        write(id_gauss_coef,'(a)',ADVANCE='NO')                         &
     &         picked_gauss_head(time_d%i_time_step, time_d%time)
        write(id_gauss_coef,fmt_txt)                                    &
     &         d_rj_out(1:gauss%istack_picked_spec_lc(nprocs))
        close(id_gauss_coef)
      end if
      deallocate(d_rj_out)
!
      end subroutine append_sph_gauss_coefs_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_sph_gauss_coefs_header                           &
     &         (id_file, ltr, nri, nlayer_ICB, nlayer_CMB, gauss)
!
      use t_solver_SR
      use set_parallel_file_name
      use delete_data_files
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(picked_spectrum_data), intent(in) :: gauss
!
      integer(kind = kint) :: i, ntot
!
!
      write(id_file,'(a)') 'radial_layers, truncation'
      write(id_file,'(3i16)') nri, ltr
      write(id_file,'(a)')  'ICB_id, CMB_id'
      write(id_file,'(2i16)') nlayer_ICB, nlayer_CMB
      write(id_file,'(a)') 'Not_used, Not_used'
      write(id_file,'(i16,1pe23.14e3)')                                 &
     &                     izero, zero
      write(id_file,'(a)') 'Not_used, reference_radius'
      write(id_file,'(i16,1pe23.14e3)')                                 &
     &                     izero, gauss%radius_gl(1)
!
      ntot = gauss%istack_picked_spec_lc(nprocs)
      write(id_file,'(a)')                                              &
     &             'number_of_gauss_coefs, number_of_gauss_coefs'
      write(id_file,'(2i16)') ntot, ntot
      write(id_file,'(16i5)') (ione,i=1,ntot)
!
      do i = 1, ntot
        write(id_file,'(a,a4)', ADVANCE='NO')                           &
     &        trim(gauss%gauss_mode_name_out(i)), '    '
      end do
      write(id_file,'(a1)', ADVANCE='NO') char(10)
!
      end subroutine write_sph_gauss_coefs_header
!
!  ---------------------------------------------------------------------
!
      subroutine write_sph_gauss_coefes(sph_params, sph_rj,             &
     &          ipol, rj_fld, gauss, ntot_gauss, d_rj_out, SR_sig)
!
      use t_solver_SR
      use pickup_gauss_coefficients
      use collect_SR_N
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: gauss
      integer(kind = kint), intent(in) :: ntot_gauss
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_gauss)
      type(send_recv_status), intent(inout) :: SR_sig
!
      integer(kind = kint_gl) :: num
      real(kind=kreal), allocatable :: d_rj_lc(:)
!
!
      num = gauss%istack_picked_spec_lc(my_rank+1)                      &
     &     - gauss%istack_picked_spec_lc(my_rank)
      allocate(d_rj_lc(gauss%num_sph_mode_lc))
      if(num .gt. 0) then
        call gauss_coefficients_4_write                                 &
     &     (sph_params, sph_rj, ipol, rj_fld, gauss, d_rj_lc)
      end if
!
      call collect_small_send_recv(gauss%istack_picked_spec_lc,         &
     &    gauss%num_sph_mode_lc, d_rj_lc,                               &
     &    gauss%istack_picked_spec_lc(nprocs), d_rj_out, SR_sig)
      deallocate(d_rj_lc)
!
      end subroutine write_sph_gauss_coefes
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      character(len = 16+25) function picked_gauss_head(i_step, time)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
!
!
      write(picked_gauss_head,'(i16,1pe25.14e3)') i_step, time
!
      end function  picked_gauss_head
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
     logical function error_gauss_coefs_header(sph_params, sph_rj,      &
    &                                          gauss)
!
      use set_parallel_file_name
      use check_sph_monitor_header
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(picked_spectrum_data), intent(in) :: gauss
!!
      character(len = kchara) :: file_name, empty_label
      integer(kind = kint) :: ntot
!
!
      error_gauss_coefs_header = .FALSE.
      if(gauss%num_sph_mode .eq. izero) return
      if(my_rank .gt. izero) return
!
      file_name = add_dat_extension(gauss%file_prefix)
      open(id_gauss_coef, file = file_name,                             &
     &    form='formatted', status='old', err = 99)
!
      empty_label = 'EMPTY'
      ntot = gauss%istack_picked_spec_lc(nprocs)
      error_gauss_coefs_header                                          &
     &         = error_sph_vol_monitor_head(id_gauss_coef, empty_label, &
     &             sph_rj%nidx_rj(1), sph_params%l_truncation,          &
     &             sph_params%nlayer_ICB, sph_params%nlayer_CMB,        &
     &             izero, zero, izero, gauss%radius_gl(1), ntot,        &
     &             gauss%ncomp_gauss_out, gauss%gauss_mode_name_out,    &
     &             ntot, gauss%gauss_mode_name_out)
!
      close(id_gauss_coef)
      return
!
  99  continue
      write(*,*) 'No Gauss coefficient file'
      return
!
      end function error_gauss_coefs_header
!
! -----------------------------------------------------------------------
!
      end module write_sph_gauss_coefs
