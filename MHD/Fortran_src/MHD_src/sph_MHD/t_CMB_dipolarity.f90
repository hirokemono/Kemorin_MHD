!>@file   t_CMB_dipolarity.f90
!!@brief      module t_CMB_dipolarity
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine dealloc_dipolarity_work(dip)
!!        type(dipolarity_data), intent(inout) :: dip
!!      subroutine set_ctl_dipolarity_params                            &
!!     &         (fdip_file_prefix, fdip_truncation, rj_fld, dip)
!!        type(read_character_item), intent(in) :: fdip_file_prefix
!!        type(read_integer_item), intent(in) :: fdip_truncation
!!        type(phys_data), intent(in) :: rj_fld
!!        type(dipolarity_data), intent(inout) :: dip
!!      subroutine write_dipolarity(i_step, time, radius_CMB,           &
!!     &                            ipol, pwr, dip)
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        real(kind = kreal), intent(in) :: radius_CMB
!!        type(phys_address), intent(in) :: ipol
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(dipolarity_data), intent(in) :: dip
!!      subroutine cal_CMB_dipolarity(nlayer_CMB, sph_params, sph_rj,   &
!!     &          ipol, g_sph_rj, rj_fld, pwr, WK_pwr, dip)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        integer(kind = kint), intent(in) :: ltr_max, nlayer_CMB
!!        real(kind = kreal), intent(in)                                &
!!     &      :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!        real(kind = kreal), intent(inout) :: f_dip
!!@endverbatim
!
      module t_CMB_dipolarity
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_phys_data
      use t_rms_4_sph_spectr
!
      implicit none
!
      type dipolarity_data
!>        Integer flag for dipolarity
        integer(kind = kint) :: iflag_dipolarity = 0
!>        File prefix for dipolarity data
        character(len = kchara) :: dipolarity_prefix= 'dipolarity'
!
!>        Radial grid point for dipolarity
        integer(kind = kint) :: kr_for_rms(1)
!>        Truncation degree to evaluate dipolarity
        integer(kind = kint) :: ltr_max
!>        magnetic energy spectrum as a function of degree
        real(kind = kreal), allocatable :: shl_l(:,:)
!>        local magnetic energy spectrum as a function of degree
        real(kind = kreal), allocatable :: shl_l_local(:,:)
!
!>        magnetic energy at CMB
        real(kind = kreal) :: me_cmb_d(3)
!>        dipole component of magnetic energy at CMB
        real(kind = kreal) :: pwr_g10(1)
!>        local dipole component of magnetic energy at CMB
        real(kind = kreal) :: pwr_g10_l(1)
!
!>        Dipolarity
        real(kind = kreal) :: f_dip
      end type dipolarity_data
!
      integer(kind = kint), parameter, private :: id_dipolarity = 36
!
      private :: alloc_dipolarity_work
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_dipolarity_work(dip)
!
      type(dipolarity_data), intent(inout) :: dip
!
      deallocate(dip%shl_l, dip%shl_l_local)
!
      end subroutine dealloc_dipolarity_work
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_dipolarity_params                              &
     &         (fdip_file_prefix, fdip_truncation, rj_fld, dip)
!
      use m_base_field_labels
      use t_phys_data
      use t_control_array_character
      use t_control_array_integer
!
      type(read_character_item), intent(in) :: fdip_file_prefix
      type(read_integer_item), intent(in) :: fdip_truncation
      type(phys_data), intent(in) :: rj_fld
      type(dipolarity_data), intent(inout) :: dip
!
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      dip%iflag_dipolarity = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          dip%iflag_dipolarity = 1
          exit
        end if
      end do
!
      if(fdip_file_prefix%iflag .gt. 0) then
        dip%iflag_dipolarity = 1
        dip%dipolarity_prefix = fdip_file_prefix%charavalue
      else
        dip%iflag_dipolarity = 0
      end if
!
      dip%ltr_max = -1
      if(fdip_truncation%iflag .gt. 0) then
        dip%ltr_max = fdip_truncation%intvalue
      end if
!
      end subroutine set_ctl_dipolarity_params
!
! -----------------------------------------------------------------------
!
      subroutine write_dipolarity(i_step, time, radius_CMB,             &
     &                            ipol, pwr, dip)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: radius_CMB
      type(phys_address), intent(in) :: ipol
      type(sph_mean_squares), intent(in) :: pwr
      type(dipolarity_data), intent(in) :: dip
!
!
      if(dip%iflag_dipolarity .le. izero) return
      if(ipol%base%i_magne .le. 0) return
      if(my_rank .ne. pwr%irank_l) return
!
      call open_dipolarity_file(dip, radius_CMB)
!
      write(id_dipolarity,'(i16,1p2e23.14e3)') i_step, time, dip%f_dip
      close(id_dipolarity)
!
      end subroutine write_dipolarity
!
! -----------------------------------------------------------------------
!
      subroutine cal_CMB_dipolarity(nlayer_CMB, sph_params, sph_rj,     &
     &          ipol, g_sph_rj, rj_fld, pwr, WK_pwr, dip)
!
      use calypso_mpi_real
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_sum_sph_rms_data
!
      use transfer_to_long_integers
      use sum_sph_rms_data
      use cal_rms_by_sph_spectr
      use cal_ave_4_rms_vector_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_shell_parameters), intent(in) :: sph_params
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: nlayer_CMB
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      type(sph_mean_squares), intent(in) :: pwr
!
      type(sph_mean_square_work), intent(inout) :: WK_pwr
      type(dipolarity_data), intent(inout) :: dip
!
      integer(kind = kint) :: j
      integer(kind = kint_gl) :: num64
!
!
!
      if(ipol%base%i_magne .le. 0) return
      if(dip%iflag_dipolarity .le. izero) return
      if(allocated(dip%shl_l)) call alloc_dipolarity_work(dip)
!
      if(dip%ltr_max .le. 0) dip%ltr_max = sph_params%l_truncation
      dip%kr_for_rms(1) = nlayer_CMB
      dip%pwr_g10(1) =    0.0d0
      dip%pwr_g10_l(1) =  0.0d0
!
      call cal_rms_sph_spec_one_field                                   &
     &   (sph_rj, ipol, n_vector, g_sph_rj, ipol%base%i_magne,          &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld,               &
     &    WK_pwr%shl_rj(0,1,1))
!
      j = find_local_sph_address(sph_rj, 1, 0)
      if(j .gt. 0)  dip%pwr_g10_l(1) = WK_pwr%shl_rj(nlayer_CMB,j,1)
      call calypso_mpi_reduce_real(dip%pwr_g10_l, dip%pwr_g10,          &
     &    cast_long(ione), MPI_SUM, pwr%irank_l)
!
!$omp parallel workshare
      dip%shl_l_local(0:dip%ltr_max,1:3) = 0.0d0
      dip%shl_l(0:dip%ltr_max,1:3) =       0.0d0
!$omp end parallel workshare
!
      call sum_sph_rms_by_degree                                        &
     &   (dip%ltr_max, sph_rj%nidx_rj, ione, dip%kr_for_rms,            &
     &    WK_pwr%istack_mode_sum_l,  WK_pwr%item_mode_sum_l,            &
     &    n_vector, WK_pwr%shl_rj, dip%shl_l_local(0,1))
!
      num64 = cast_long(3 * (dip%ltr_max + 1))
      call calypso_mpi_reduce_real(dip%shl_l_local, dip%shl_l, num64,   &
     &                             MPI_SUM, pwr%irank_l)
!
      dip%me_cmb_d(1:3) = 0.0d0
      if(my_rank .eq. pwr%irank_l) then
        call sum_sph_rms_all_modes                                      &
     &     (dip%ltr_max, ione, ithree, dip%shl_l, dip%me_cmb_d(1))
!
        dip%f_dip = dip%pwr_g10(1) / dip%me_cmb_d(1)
      end if
!
      end subroutine cal_CMB_dipolarity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_dipolarity_work(dip)
!
      type(dipolarity_data), intent(inout) :: dip
!
      allocate(dip%shl_l_local(0:dip%ltr_max,3))
      allocate(dip%shl_l(0:dip%ltr_max,3))
!
      end subroutine alloc_dipolarity_work
!
! -----------------------------------------------------------------------
!
      subroutine open_dipolarity_file(dip, radius_CMB)
!
      use set_parallel_file_name
      use write_field_labels
!
      type(dipolarity_data), intent(in) :: dip
      real(kind = kreal), intent(in) :: radius_CMB
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(dip%dipolarity_prefix)
      open(id_dipolarity, file = file_name,                             &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_dipolarity, file = file_name,                             &
     &    form='formatted', status='replace')
!
!
      write(id_dipolarity,'(a)')    '# Truncation   CMB_radius'
      write(id_dipolarity,'(1p2e25.15e3)')                              &
     &                         dip%ltr_max, radius_CMB
!
      write(id_dipolarity,'(a)',advance='NO')                           &
     &    't_step    time    f_dip'
      write(id_dipolarity,'(a)') ''
!
      end subroutine open_dipolarity_file
!
! -----------------------------------------------------------------------
!
      end module t_CMB_dipolarity
