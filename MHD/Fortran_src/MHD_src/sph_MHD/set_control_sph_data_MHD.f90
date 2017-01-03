!>@file   set_control_sph_data_MHD.f90
!!@brief  module set_control_sph_data_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2009
!
!>@brief  Set control parameters for spherical harmonics dynamo from IO
!!
!!@verbatim
!!      subroutine s_set_control_sph_data_MHD                           &
!!     &         (plt, rj_org_param, rst_org_param, rj_fld)
!!       type(field_IO_params), intent(in) :: rj_org_param
!!       type(field_IO_params), intent(in) :: rst_org_param
!!       type(phys_data), intent(inout) :: rj_fld
!!     subroutine set_ctl_params_pick_circle(meq_ctl)
!!        type(mid_equator_control), intent(in) :: meq_ctl
!!     subroutine set_ctl_params_dynamobench(meq_ctl)
!!        type(mid_equator_control), intent(in) :: meq_ctl
!!@endverbatim
!
      module set_control_sph_data_MHD
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_sph_data_MHD                             &
     &         (plt, rj_org_param, rst_org_param, rj_fld)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use t_ctl_data_4_platforms
      use m_ctl_data_4_fields
      use m_ctl_data_mhd_forces
      use m_ctl_data_mhd_evo_scheme
      use m_physical_property
      use m_file_format_switch
!
      use m_sph_boundary_input_data
      use m_sel_spherical_SRs
      use m_FFT_selector
!
      use t_phys_data
      use t_field_data_IO
!
      use skip_comment_f
      use set_control_sph_data
      use legendre_transform_select
      use add_nodal_fields_4_MHD
      use add_sph_MHD_fields_2_ctl
      use sph_mhd_rst_IO_control
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(in) :: rj_org_param, rst_org_param
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: ierr
!
!   overwrite restart header for magnetic field extension
!
      call set_rst_file_by_orignal_mesh(rj_org_param, rst_org_param)
!
!   set physical values
!
      if(field_ctl%icou .eq. 0) then
        call calypso_MPI_abort(ierr_fld, 'Set field for simulation')
      end if
      if (iflag_debug.eq.1) write(*,*)                                  &
     &    'original number of field ', field_ctl%num
!
      if ( field_ctl%num .ne. 0 ) then
!
!     add fields for simulation
!
        call add_field_name_4_mhd
        call add_field_name_4_sph_mhd
        call add_field_name_4_SGS
        call add_field_name_dynamic_SGS
        if (iflag_debug.eq.1) write(*,*)                                &
     &    'field_ctl%num after modified ', field_ctl%num
!
!    set nodal data
!
        if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data'
        call s_set_control_sph_data(rj_fld, ierr)
      end if
!
!
      if(legendre_vector_len_ctl%iflag .gt. 0) then
        nvector_legendre = legendre_vector_len_ctl%intvalue
      else
        nvector_legendre = 0
      end if
!      
      if(Legendre_trans_loop_ctl%iflag .gt. 0) then
        call set_legendre_trans_mode_ctl                                &
     &     (Legendre_trans_loop_ctl%charavalue)
      end if
!
      if(FFT_library_ctl%iflag .gt. 0) then
        call set_fft_library_ctl(FFT_library_ctl%charavalue)
      end if
      if(import_mode_ctl%iflag .gt. 0) then
        call set_import_table_ctl(import_mode_ctl%charavalue)
      end if
      if(SR_routine_ctl%iflag .gt. 0) then
        call set_sph_comm_routine_ctl(SR_routine_ctl%charavalue)
      end if
!
      if (plt%bc_data_file_name_ctl%iflag .gt. 0) then
        bc_sph_file_name = plt%bc_data_file_name_ctl%charavalue
      end if
!
      end subroutine s_set_control_sph_data_MHD
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_dynamobench(meq_ctl)
!
      use t_ctl_data_sph_vol_spectr
      use m_ctl_data_4_fields
      use m_phys_labels
      use m_phys_constants
      use m_field_on_circle
      use m_circle_transform
      use m_field_at_mid_equator
!
      type(mid_equator_control), intent(in) :: meq_ctl
!
      integer(kind = kint) :: ifld
!
!
      iflag_circle_coord = iflag_circle_sph
!
      mphi_circle = -1
      if(meq_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        mphi_circle = meq_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      do ifld = 1, field_ctl%num
        if(field_ctl%c1_tbl(ifld) .eq. fhd_temp) ibench_temp = 1
        if(field_ctl%c1_tbl(ifld) .eq. fhd_velo) ibench_velo = 1
        if(field_ctl%c1_tbl(ifld) .eq. fhd_magne) ibench_magne = 1
      end do
!
      d_circle%num_phys = ibench_velo + ibench_temp + ibench_magne
      call alloc_phys_name_type(d_circle)
!
      ifld = 0
      if(ibench_temp .gt. 0) then
        ifld = ifld + 1
        ibench_temp = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     fhd_temp
        d_circle%num_component(ifld) = n_scalar
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_scalar
      end if
      if(ibench_velo .gt. 0) then
        ifld = ifld + 1
        ibench_velo = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     fhd_velo
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      if(ibench_magne .gt. 0) then
        ifld = ifld + 1
        ibench_magne = d_circle%istack_component(ifld-1) + 1
        d_circle%phys_name(ifld) =     fhd_magne
        d_circle%num_component(ifld) = n_vector
        d_circle%istack_component(ifld)                                 &
     &        = d_circle%istack_component(ifld-1) + n_vector
      end if
      d_circle%iflag_monitor = ione
      d_circle%ntot_phys =     d_circle%istack_component(ifld)
      d_circle%num_phys_viz =  d_circle%num_phys
      d_circle%ntot_phys_viz = d_circle%ntot_phys
!
      end subroutine set_ctl_params_dynamobench
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_pick_circle(meq_ctl)
!
      use t_ctl_data_sph_vol_spectr
      use m_ctl_data_4_fields
      use m_field_on_circle
      use m_circle_transform
      use t_phys_data
      use ordering_field_by_viz
      use skip_comment_f
!
      type(mid_equator_control), intent(in) :: meq_ctl
!
      character(len = kchara) :: tmpchara
!
!
      iflag_circle_coord = iflag_circle_sph
      if (meq_ctl%pick_circle_coord_ctl%iflag .ne. 0) then
        tmpchara = meq_ctl%pick_circle_coord_ctl%charavalue
        if(    cmp_no_case(tmpchara,'spherical')                        &
     &    .or. cmp_no_case(tmpchara,'rtp')) then
          iflag_circle_coord = iflag_circle_sph
        else if(cmp_no_case(tmpchara,'cyrindrical')                     &
      &    .or. cmp_no_case(tmpchara,'spz')) then
          iflag_circle_coord = iflag_circle_cyl
        end if
      end if
!
      mphi_circle = -1
      if(meq_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        mphi_circle = meq_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      s_circle = 7.0d0/13.0d0 + 0.5d0
      if(meq_ctl%pick_s_ctl%iflag .gt. 0) then
        s_circle = meq_ctl%pick_s_ctl%realvalue
      end if
!
      z_circle = 0.0d0
      if(meq_ctl%pick_z_ctl%iflag .gt. 0) then
        z_circle = meq_ctl%pick_z_ctl%realvalue
      end if
!
      d_circle%num_phys = field_ctl%num
      call alloc_phys_name_type(d_circle)
      call s_ordering_field_by_viz(d_circle%num_phys,                   &
     &    d_circle%num_phys_viz, d_circle%num_component,                &
     &    d_circle%phys_name, d_circle%iflag_monitor)
!
      call set_istack_4_nodal_field(d_circle%num_phys,                  &
     &    d_circle%num_phys_viz, d_circle%num_component,                &
     &    d_circle%ntot_phys, d_circle%ntot_phys_viz,                   &
     &    d_circle%istack_component)
!
      end subroutine set_ctl_params_pick_circle
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module set_control_sph_data_MHD
