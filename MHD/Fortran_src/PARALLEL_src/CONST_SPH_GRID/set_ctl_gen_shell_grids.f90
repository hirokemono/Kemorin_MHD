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
!!     &         (sph_params, sph_rtp, sph_rj)
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!@endverbatim
!
      module set_ctl_gen_shell_grids
!
      use m_precision
!
      use t_spheric_parameter
      use t_field_data_IO
!
      implicit  none
!
      type(field_IO_params), save, private :: sph_file_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_control_4_gen_shell_grids                        &
     &         (sph_params, sph_rtp, sph_rj)
!
      use m_constants
      use m_machine_parameter
      use m_read_mesh_data
      use m_spheric_constants
      use m_spheric_global_ranks
      use m_sph_1d_global_index
      use m_sph_mesh_1d_connect
!
      use m_node_id_spherical_IO
      use m_file_format_switch
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_sphere_model
      use m_ctl_data_4_divide_sphere
!
      use set_controls_4_sph_shell
      use const_sph_radial_grid
      use set_control_platform_data
      use gen_sph_grids_modes
      use skip_comment_f
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: nprocs_ctl
      integer(kind = kint) :: i, np, kr, icou
      real(kind = kreal) :: ICB_to_CMB_ratio, fluid_core_size
!
!
      nprocs_ctl = 1
      if(ndomain_ctl%iflag .gt. 0) nprocs_ctl = ndomain_ctl%intvalue
      call turn_off_debug_flag_by_ctl(izero)
      call set_control_mesh_def
      call set_control_sph_mesh(sph_file_param)
!
      iflag_excluding_FEM_mesh = 0
      if(excluding_FEM_mesh_ctl%iflag .gt. 0                            &
     &    .and. yes_flag(excluding_FEM_mesh_ctl%charavalue)) then
        iflag_excluding_FEM_mesh = 1
      end if
!
      call set_FEM_mesh_mode_4_SPH(sph_params%iflag_shell_mode)
!
      sph_rtp%nidx_global_rtp(1) = 2
      sph_rtp%nidx_global_rtp(2) = 2
      sph_rtp%nidx_global_rtp(3) = 4
      sph_params%l_truncation = 2
      sph_params%m_folding =    1
!
      sph_params%iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_explicit))  &
     &       sph_params%iflag_radial_grid =  igrid_non_euqidist
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_Chebyshev)) &
     &       sph_params%iflag_radial_grid =  igrid_Chebyshev
      if(cmp_no_case(radial_grid_type_ctl%charavalue, label_equi))      &
     &       sph_params%iflag_radial_grid =  igrid_euqidistance
!
      if (ltr_ctl%iflag .gt. 0) then
        sph_params%l_truncation = ltr_ctl%intvalue
      end if
!
      if (phi_symmetry_ctl%iflag .gt. 0) then
        sph_params%m_folding = phi_symmetry_ctl%intvalue
      end if
!
      if (ngrid_elevation_ctl%iflag .gt. 0) then
        sph_rtp%nidx_global_rtp(2) = ngrid_elevation_ctl%intvalue
      end if
!
!      if (ngrid_azimuth_ctl%iflag .gt. 0) then
!        sph_rtp%nidx_global_rtp(3) = ngrid_azimuth_ctl%intvalue
!      end if
!
!   Set radial group
      if(radial_grp_ctl%icou .gt. 0) then
        numlayer_sph_bc = radial_grp_ctl%num
      else
        numlayer_sph_bc = 0
      end if
      call allocate_sph_radial_group
!
      icou = 0
      do i = 1, numlayer_sph_bc
        if     (cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      ICB_nod_grp_name)) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      CMB_nod_grp_name)) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i),                    &
     &                      CTR_nod_grp_name)) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else if(cmp_no_case(radial_grp_ctl%c_tbl(i), 'Mid_Depth')) then
          numlayer_sph_bc = numlayer_sph_bc - 1
        else
          icou = icou + 1
          kr_sph_boundary(icou) =  radial_grp_ctl%ivec(i)
          sph_bondary_name(icou) = radial_grp_ctl%c_tbl(i)
        end if
      end do
!
!   Set radial grid explicitly
      sph_rj%iflag_rj_center = 0
      if(sph_params%iflag_radial_grid .eq. igrid_non_euqidist) then
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
          stop
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
      ndomain_rtp(1:3) = 1
      if (ndomain_sph_grid_ctl%num .gt. 0) then
        do i = 1, ndomain_sph_grid_ctl%num
          if     (cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), 'r')       &
     &       .or. cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), 'radial')  &
     &           ) then
            ndomain_rtp(1) = ndomain_sph_grid_ctl%ivec(i)
          else if (cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), 'theta')  &
     &     .or. cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i),'meridional') &
     &           ) then
            ndomain_rtp(2) = ndomain_sph_grid_ctl%ivec(i)
          end if
        end do
!
        call deallocate_ndomain_rtp_ctl
      end if
!
      ndomain_rtm(1:3) = 1
      if (ndomain_legendre_ctl%num .gt. 0) then
        do i = 1, ndomain_legendre_ctl%num
          if     (cmp_no_case(ndomain_legendre_ctl%c_tbl(i), 'r')       &
     &       .or. cmp_no_case(ndomain_legendre_ctl%c_tbl(i), 'radial')  &
     &           ) then
            ndomain_rtm(1) = ndomain_legendre_ctl%ivec(i)
          else if (cmp_no_case(ndomain_legendre_ctl%c_tbl(i), 'phi')    &
     &     .or. cmp_no_case(ndomain_legendre_ctl%c_tbl(i),'zonal')      &
     &           ) then
            ndomain_rtm(3) = ndomain_legendre_ctl%ivec(i)
           end if
        end do
!
        call deallocate_ndomain_rtm_ctl
      end if
!
      ndomain_rlm(1) = ndomain_rtm(1)
      ndomain_rlm(2) = ndomain_rtm(3)
!
      ndomain_rj(1:2) = 1
      if (ndomain_spectr_ctl%num .gt. 0) then
        do i = 1, ndomain_spectr_ctl%num
          if(cmp_no_case(ndomain_spectr_ctl%c_tbl(i), 'degree_order')   &
     &       .or. cmp_no_case(ndomain_spectr_ctl%c_tbl(i),'modes')      &
     &      ) ndomain_rj(2) = ndomain_spectr_ctl%ivec(i)
        end do
!
        call deallocate_ndomain_rj_ctl
      end if
!
!
      ndomain_sph = ndomain_rj(1)*ndomain_rj(2)
      if (ndomain_sph .ne. nprocs_ctl) then
        write(*,*) 'check num of domain spectr file(r,j)'
        stop
      end if
!
      np = ndomain_rtp(1)*ndomain_rtp(2)*ndomain_rtp(3)
      if (ndomain_sph .ne. np) then
        write(*,*) 'check num of domain for (r,t,p)'
        stop
      end if
      np = ndomain_rtm(1)*ndomain_rtm(2)*ndomain_rtm(3)
      if (ndomain_sph .ne. np) then
        write(*,*) 'check num of domain for (r,t,m)'
        stop
      end if
      np = ndomain_rlm(1)*ndomain_rlm(2)
      if (ndomain_sph .ne. np) then
        write(*,*) 'check num of domain for (r,l,m)'
        stop
      end if
!
      if(ndomain_rtm(1) .ne. ndomain_rtp(1)) then
        write(*,*) 'Set same number of radial subdomains'
        write(*,*) 'for Legendre transform and spherical grids'
        stop
      end if
!
      if(mod(sph_rtp%nidx_global_rtp(3),2) .ne. 0) then
        write(*,*) 'Set even number for the number of zonal grids'
        stop
      end if
!
      if(sph_rtp%nidx_global_rtp(2)                                     &
     &      .lt. (sph_params%l_truncation+1)*3/2) then
        write(*,*) 'Spherical harmonics transform has Ailiasing'
      else if (sph_rtp%nidx_global_rtp(2)                               &
     &      .lt. (sph_params%l_truncation+1)) then
        write(*,*) "Grid has less than Nyquist's sampling theorem"
      end if
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'icou, kr_sph_boundary, sph_bondary_name',           &
     &             numlayer_sph_bc
        do icou = 1, numlayer_sph_bc
          write(*,*) icou, kr_sph_boundary(icou),                       &
     &               trim(sph_bondary_name(icou))
        end do
      end if
!
      end subroutine s_set_control_4_gen_shell_grids
!
!  ---------------------------------------------------------------------
!
      end module set_ctl_gen_shell_grids
