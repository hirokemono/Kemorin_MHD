!>@file   t_no_heat_Nusselt.f90
!!@brief  module t_no_heat_Nusselt
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for Nusselt number
!!
!!@verbatim
!!      subroutine alloc_Nu_radial_reference(sph_rj, Nu_type)
!!      subroutine dealloc_Nu_radial_reference(Nu_type)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!
!!      subroutine set_ctl_params_no_heat_Nu                            &
!!     &         (source_name, Nusselt_file_prefix, rj_fld, Nu_type)
!!        character(len = kchara) :: source_name
!!        type(read_character_item), intent(in) :: Nusselt_file_prefix
!!        type(phys_data), intent(in) :: rj_fld
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!
!!      subroutine write_Nusselt_file(i_step, time, ltr, nri,           &
!!     &          nlayer_ICB, nlayer_CMB, idx_rj_degree_zero, Nu_type)
!!      subroutine write_Nu_header(id_file, ltr, nri,                   &
!!     &                           nlayer_ICB, nlayer_CMB, Nu_type)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: ltr, nri
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(nusselt_number_data), intent(in) :: Nu_type
!!@endverbatim
!
      module t_no_heat_Nusselt
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>      File ID for Nusselt number IO
      integer(kind = kint), parameter :: id_Nusselt = 23
!
!>        Output flag for Nusselt number IO
      integer(kind = kint), parameter :: iflag_no_source_Nu = 1
!>        Output flag for Nusselt number IO
      integer(kind = kint), parameter :: iflag_source_Nu = 2
!
!>      Structure for Nusselt number data
      type nusselt_number_data
!>        Output flag for Nusselt number IO
        integer(kind = kint) :: iflag_Nusselt = 0
!>        File name for Nusselt number file
        character(len = kchara) :: Nusselt_file_name = 'Nusselt.dat'
!
!>        Radius ID at inner boundary
        integer(kind = kint) :: kr_ICB_Nu
!>        Radius ID at outer boundary
        integer(kind = kint) :: kr_CMB_Nu
!>        Radius at inner boundary
        real(kind = kreal) :: r_ICB_Nu
!>        Radius at outer boundary
        real(kind = kreal) :: r_CMB_Nu
!>        Nusselt number at inner boundary
        real(kind = kreal) :: Nu_ICB
!>        Nusselt number at outer boundary
        real(kind = kreal) :: Nu_CMB
!
        integer(kind = kint) :: nri_w_ctr
!>        diffusive profile and derivative
        real(kind = kreal), allocatable :: ref_global(:,:)
!>        local diffusive profile and derivative
        real(kind = kreal), allocatable :: ref_local(:,:)
      end type nusselt_number_data
!
      private :: id_Nusselt
      private :: open_Nu_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_Nu_radial_reference(sph_rj, Nu_type)
! 
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(nusselt_number_data), intent(inout) :: Nu_type
!
!
      if(size(Nu_type%ref_global,1) .eq. sph_rj%nidx_rj(1)) return
      Nu_type%nri_w_ctr = sph_rj%nidx_rj(1)
!
      allocate(Nu_type%ref_global(0:Nu_type%nri_w_ctr,0:1))
      allocate(Nu_type%ref_local(0:Nu_type%nri_w_ctr,0:1))
!
!$omp parallel workshare
      Nu_type%ref_global(0:Nu_type%nri_w_ctr,0:1) = 0.0d0
      Nu_type%ref_local(0:Nu_type%nri_w_ctr,0:1) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_Nu_radial_reference
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_Nu_radial_reference(Nu_type)
!
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      if(allocated(Nu_type%ref_global) .eqv. .FALSE.) return
      deallocate(Nu_type%ref_global)
      deallocate(Nu_type%ref_local)
!
      end subroutine dealloc_Nu_radial_reference
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_no_heat_Nu                              &
     &         (source_name, Nusselt_file_prefix, rj_fld, Nu_type)
!
      use m_base_field_labels
      use m_grad_field_labels
      use t_phys_data
      use t_control_array_character
      use set_parallel_file_name
!
      character(len = kchara) :: source_name
      type(read_character_item), intent(in) :: Nusselt_file_prefix
      type(phys_data), intent(in) :: rj_fld
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      character(len = kchara) :: file_prfix
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      Nu_type%iflag_Nusselt = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. grad_temp%name) then
          Nu_type%iflag_Nusselt = iflag_no_source_Nu
          exit
        end if
      end do
!
      if(Nusselt_file_prefix%iflag .gt. 0) then
        Nu_type%iflag_Nusselt = iflag_no_source_Nu
        file_prfix = Nusselt_file_prefix%charavalue
        Nu_type%Nusselt_file_name = add_dat_extension(file_prfix)
      else
        Nu_type%iflag_Nusselt = 0
      end if
!
!    Turn Off Nusselt number if heat or composition source is there
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. source_name) then
!          Nu_type%iflag_Nusselt = iflag_source_Nu
          Nu_type%iflag_Nusselt = 0
          exit
        end if
      end do
!
      end subroutine set_ctl_params_no_heat_Nu
!
! -----------------------------------------------------------------------
!
      subroutine open_Nu_file(ltr, nri, nlayer_ICB, nlayer_CMB, Nu_type)
!
      use set_parallel_file_name
      use write_field_labels
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(nusselt_number_data), intent(in) :: Nu_type
!
!
      open(id_Nusselt, file = Nu_type%Nusselt_file_name,                &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_Nusselt, file = Nu_type%Nusselt_file_name,                &
     &    form='formatted', status='replace')
!
      call write_Nu_header(id_Nusselt, ltr, nri,                        &
     &                     nlayer_ICB, nlayer_CMB, Nu_type)
!
      end subroutine open_Nu_file
!
! -----------------------------------------------------------------------
!
      subroutine write_Nusselt_file(i_step, time, ltr, nri,             &
     &          nlayer_ICB, nlayer_CMB, idx_rj_degree_zero, Nu_type)
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: idx_rj_degree_zero, i_step
      real(kind = kreal), intent(in) :: time
      type(nusselt_number_data), intent(in) :: Nu_type
!
!
      if(Nu_type%iflag_Nusselt .eq. izero) return
      if(idx_rj_degree_zero .eq. izero) return
!
      call open_Nu_file(ltr, nri, nlayer_ICB, nlayer_CMB, Nu_type)
!
      write(id_Nusselt,'(i16,1p3e23.14e3)')                             &
     &       i_step, time, Nu_type%Nu_ICB, Nu_type%Nu_CMB
!
      close(id_Nusselt)
!
      end subroutine write_Nusselt_file
!
! -----------------------------------------------------------------------
!
      subroutine write_Nu_header(id_file, ltr, nri,                     &
     &                           nlayer_ICB, nlayer_CMB, Nu_type)
!
      use t_read_sph_spectra
      use sph_power_spectr_data_text
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(nusselt_number_data), intent(in) :: Nu_type
!
      type(read_sph_spectr_data) :: sph_OUT
      integer(kind = kint) :: len_tot
      integer(kind = kint) :: len_each(6)
!
!
!
      call dup_Nusselt_header_to_IO                                     &
     &   (ltr, nri, nlayer_ICB, nlayer_CMB, Nu_type, sph_OUT)
!
      call len_sph_vol_spectr_header(sph_pwr_labels, sph_OUT,           &
     &                               len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &       sph_vol_spectr_header_text(len_tot, len_each,              &
     &                                  sph_pwr_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_OUT)
      call dealloc_sph_espec_name(sph_OUT)
!
      end subroutine write_Nu_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_Nusselt_header_to_IO                               &
     &         (ltr, nri, nlayer_ICB, nlayer_CMB, Nu_type, sph_OUT)
!
      use t_read_sph_spectra
      use m_time_labels
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(nusselt_number_data), intent(in) :: Nu_type
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: icou, ntot
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = nri
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
      sph_OUT%kr_inner = Nu_type%kr_ICB_Nu
      sph_OUT%kr_outer = Nu_type%kr_CMB_Nu
      sph_OUT%r_inner =  Nu_type%r_ICB_Nu
      sph_OUT%r_outer =  Nu_type%r_CMB_Nu
!
      sph_OUT%nfield_sph_spec = 2
      sph_OUT%ntot_sph_spec =   2
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
      call alloc_sph_spectr_data(izero, sph_OUT)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      sph_OUT%ene_sph_spec_name(3) = 'Nu_ICB'
      sph_OUT%ene_sph_spec_name(4) = 'Nu_CMB'
      icou = sph_OUT%num_time_labels
      sph_OUT%ncomp_sph_spec(1:sph_OUT%nfield_sph_spec) = 1
!
      end subroutine dup_Nusselt_header_to_IO
!
! -----------------------------------------------------------------------
!
      end module t_no_heat_Nusselt
