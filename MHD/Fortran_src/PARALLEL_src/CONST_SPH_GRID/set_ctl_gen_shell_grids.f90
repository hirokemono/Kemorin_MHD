!>@file   set_ctl_gen_shell_grids.f90
!!@brief  module set_ctl_gen_shell_grids
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine s_set_control_4_gen_shell_grids                      &
!!     &         (sph, sph_file_param, ierr)
!!      subroutine set_control_4_shell_grids(nprocs_check, sph, ierr)
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!        type(sph_grids), intent(inout) :: sph
!!@endverbatim
!
      module set_ctl_gen_shell_grids
!
      use m_precision
!
      use t_spheric_parameter
      use t_file_IO_parameter
!
      implicit  none
!
      character(len=kchara), parameter :: cflag_SGS_r = 'SGS_r'
      character(len=kchara), parameter :: cflag_SGS_t = 'SGS_theta'
!
      private :: cflag_SGS_r, cflag_SGS_t
      private :: set_control_4_shell_filess
      private :: set_ctl_radius_4_shell, set_control_4_SGS_shell
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_control_4_gen_shell_grids                        &
     &         (sph, sph_file_param, ierr)
!
      type(sph_grids), intent(inout) :: sph
      type(field_IO_params), intent(inout) :: sph_file_param
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: nprocs_check
!
!
      call set_control_4_shell_filess(nprocs_check, sph_file_param)
!
      call set_control_4_shell_grids(nprocs_check, sph, ierr)
!
      end subroutine s_set_control_4_gen_shell_grids
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_shell_filess                             &
     &         (nprocs_check, sph_file_param)
!
      use m_ctl_data_4_platforms
      use set_control_platform_data
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(inout) :: nprocs_check
      type(field_IO_params), intent(inout) :: sph_file_param
!
!
      nprocs_check = 1
      if(ndomain_ctl%iflag .gt. 0) nprocs_check = ndomain_ctl%intvalue
      call turn_off_debug_flag_by_ctl(izero)
      call set_control_mesh_def
      call set_FEM_mesh_switch_4_SPH(iflag_output_mesh)
      call set_control_sph_mesh(sph_file_param)
!
      end subroutine set_control_4_shell_filess
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_shell_grids(nprocs_check, sph, ierr)
!
      use m_constants
      use m_machine_parameter
      use m_read_mesh_data
      use m_spheric_constants
      use m_spheric_global_ranks
      use m_sph_1d_global_index
      use m_sph_mesh_1d_connect
      use m_error_IDs
!
      use m_file_format_switch
      use m_ctl_data_4_sphere_model
!
      use set_controls_4_sph_shell
      use set_control_sph_subdomains
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: nprocs_check
      type(sph_grids), intent(inout) :: sph
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: icou
!
!
      call set_FEM_mesh_mode_4_SPH(sph%sph_params%iflag_shell_mode)
!
      sph%sph_rtp%nidx_global_rtp(1) = 2
      sph%sph_rtp%nidx_global_rtp(2) = 2
      sph%sph_rtp%nidx_global_rtp(3) = 4
      sph%sph_params%l_truncation = 2
      sph%sph_params%m_folding =    1
!
      if (ltr_ctl%iflag .gt. 0) then
        sph%sph_params%l_truncation = ltr_ctl%intvalue
      end if
!
      if (phi_symmetry_ctl%iflag .gt. 0) then
        sph%sph_params%m_folding = phi_symmetry_ctl%intvalue
      end if
!
      if (ngrid_elevation_ctl%iflag .gt. 0) then
        sph%sph_rtp%nidx_global_rtp(2) = ngrid_elevation_ctl%intvalue
      end if
!
!      if (ngrid_azimuth_ctl%iflag .gt. 0) then
!        sph%sph_rtp%nidx_global_rtp(3) = ngrid_azimuth_ctl%intvalue
!      end if
!
      call set_ctl_radius_4_shell                                      &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rj, ierr)
!
      call set_subdomains_4_sph_shell(nprocs_check, ierr, e_message)
      if (ierr .gt. 0) return
!
!
!   Set layering parameter for SGS models
      call set_control_4_SGS_shell(sph)
!
!  Check
      if    (sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole     &
     &  .or. sph%sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
        if(mod(sph%sph_rtp%nidx_global_rtp(3),2) .ne. 0) then
          write(*,*) 'Set even number for the number of zonal grids'
          ierr = ierr_mesh
          return
        end if
      end if
!
      if(sph%sph_rtp%nidx_global_rtp(2)                                 &
     &      .lt. (sph%sph_params%l_truncation+1)*3/2) then
        write(*,*) 'Spherical harmonics transform has Ailiasing'
      else if (sph%sph_rtp%nidx_global_rtp(2)                           &
     &      .lt. (sph%sph_params%l_truncation+1)) then
        write(*,*) "Grid has less than Nyquist's sampling theorem"
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'icou, kr_sph_boundary, sph_bondary_name',           &
     &             added_radial_grp%nlayer
        do icou = 1, added_radial_grp%nlayer
          write(*,*) icou, added_radial_grp%istart(icou),               &
     &               trim(added_radial_grp%name(icou))
        end do
      end if
!
      end subroutine set_control_4_shell_grids
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_radius_4_shell                              &
     &         (sph_params, sph_rtp, sph_rj, ierr)
!
      use m_sph_1d_global_index
      use m_sph_mesh_1d_connect
      use m_error_IDs
!
      use m_ctl_data_4_sphere_model
      use t_control_1D_layering
!
      use const_sph_radial_grid
      use skip_comment_f
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rj_grid), intent(inout) :: sph_rj
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: i, kr, icou
      real(kind = kreal) :: ICB_to_CMB_ratio, fluid_core_size
!
!
      sph_params%iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_explicit))  &
     &       sph_params%iflag_radial_grid =  igrid_non_equidist
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_Chebyshev)) &
     &       sph_params%iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_equi))      &
     &       sph_params%iflag_radial_grid =  igrid_equidistance
!
!   Set radial group
      if(radial_grp_ctl%icou .le. 0) added_radial_grp%nlayer = 0
      call alloc_layering_group(radial_grp_ctl%num, added_radial_grp)
!
      icou = 0
      do i = 1, added_radial_grp%nlayer
        if     (cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      ICB_nod_grp_name)) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      CMB_nod_grp_name)) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      CTR_nod_grp_name)) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'Mid_Depth')) then
          added_radial_grp%nlayer = added_radial_grp%nlayer - 1
        else
          icou = icou + 1
          added_radial_grp%istart(icou) =  radial_grp_ctl%ivec(i)
          added_radial_grp%iend(icou) =    radial_grp_ctl%ivec(i)
          added_radial_grp%name(icou) =    radial_grp_ctl%c_tbl(i)
        end if
      end do
!
!   Set radial grid explicitly
      sph_rj%iflag_rj_center = 0
      if(sph_params%iflag_radial_grid .eq. igrid_non_equidist) then
        if(cmp_no_case(sph_coef_type_ctl%charavalue, 'with_center')     &
          .and. sph_coef_type_ctl%iflag .gt. 0) then
          sph_rj%iflag_rj_center = 1
        end if
!
        if (radius_ctl%icou .gt. 0) then
          sph_rtp%nidx_global_rtp(1) = radius_ctl%num
        end if
!
        if (sph_rtp%nidx_global_rtp(1) .gt. 0) then
          call allocate_radius_1d_gl(sph_rtp%nidx_global_rtp(1))
!
          do i = 1, sph_rtp%nidx_global_rtp(1)
            kr = radius_ctl%ivec(i)
            radius_1d_gl(kr) = radius_ctl%vect(i)
          end do
!
          call dealloc_control_array_i_r(radius_ctl)
        end if
!
        sph_params%nlayer_2_center = -1
        sph_params%nlayer_ICB =       1
        sph_params%nlayer_CMB = sph_rtp%nidx_global_rtp(1)
        sph_params%nlayer_mid_OC =   -1
        if(radial_grp_ctl%icou .gt. 0) then
          do i = 1, radial_grp_ctl%num
            if     (cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      ICB_nod_grp_name) ) then
              sph_params%nlayer_ICB = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      CMB_nod_grp_name) ) then
              sph_params%nlayer_CMB = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      CTR_nod_grp_name) ) then
              sph_params%nlayer_2_center = radial_grp_ctl%ivec(i)
            else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                &
     &                      'Mid_Depth') ) then
              sph_params%nlayer_mid_OC = radial_grp_ctl%ivec(i)
            end if
          end do
!
          call dealloc_control_array_c_i(radial_grp_ctl)
        end if
!
!   Set radial grid by Chebyshev or equaidistance
      else
        if(ICB_radius_ctl%iflag .gt. 0                                  &
     &     .and. CMB_radius_ctl%iflag .gt. 0) then
          sph_params%radius_ICB = ICB_radius_ctl%realvalue
          sph_params%radius_CMB = CMB_radius_ctl%realvalue
        else if(fluid_core_size_ctl%iflag .gt. 0                        &
     &       .and. ICB_to_CMB_ratio_ctl%iflag .gt. 0) then
          ICB_to_CMB_ratio = ICB_to_CMB_ratio_ctl%realvalue
          fluid_core_size =  fluid_core_size_ctl%realvalue
          sph_params%radius_ICB = fluid_core_size                       &
     &           * ICB_to_CMB_ratio / (one - ICB_to_CMB_ratio)
          sph_params%radius_CMB = sph_params%radius_ICB                 &
     &                           + fluid_core_size
        else
          write(*,*)                                                    &
     &       'Set CMB and ICB radii or ratio and size of outer core'
          ierr = ierr_mesh
          return
        end if
!
        if(Min_radius_ctl%iflag.eq.0) then
          Min_radius_ctl%realvalue = sph_params%radius_ICB
        end if
        if(Max_radius_ctl%iflag.eq.0) then
          Max_radius_ctl%realvalue = sph_params%radius_CMB
        end if
!
        if(Min_radius_ctl%realvalue .eq. zero) then
          sph_rj%iflag_rj_center = 1
        end if
!
        call count_set_radial_grid(num_fluid_grid_ctl%intvalue,         &
     &      Min_radius_ctl%realvalue, Max_radius_ctl%realvalue,         &
     &      sph_params, sph_rtp)
      end if
!
      end subroutine set_ctl_radius_4_shell
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_SGS_shell(sph)
!
      use m_sph_1d_global_index
      use m_ctl_data_4_sphere_model
!
      use t_control_1D_layering
!
      type(sph_grids), intent(in) :: sph
!
!
!   Set layering parameter for SGS models
      if(radial_layer_list_ctl%num .gt. 0) then
        call set_group_by_layering_list                                 &
     &    (cflag_SGS_r, radial_layer_list_ctl, r_layer_grp)
      else if(num_radial_layer_ctl%iflag .gt. 0) then
        call set_group_by_equidivide(cflag_SGS_r,                       &
     &      sph%sph_params%nlayer_ICB, sph%sph_params%nlayer_CMB,       &
     &      num_radial_layer_ctl, r_layer_grp)
      else
        call alloc_layering_group(izero, r_layer_grp)
      end if
!
      if(med_layer_list_ctl%num .gt. 0) then
        call set_group_by_layering_list                                 &
     &    (cflag_SGS_t, med_layer_list_ctl, med_layer_grp)
      else if(num_med_layer_ctl%iflag .gt. 0) then
        call set_group_by_equidivide                                    &
     &    (cflag_SGS_t, ione, sph%sph_rtp%nidx_global_rtp(2),           &
     &      num_med_layer_ctl, med_layer_grp)
      else
        call alloc_layering_group(izero, med_layer_grp)
      end if
!
      end subroutine set_control_4_SGS_shell
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_gen_shell_grids
