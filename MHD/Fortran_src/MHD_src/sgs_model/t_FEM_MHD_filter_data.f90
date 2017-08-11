!t_FEM_MHD_filter_data.f90
!      module t_FEM_MHD_filter_data
!
!     Written by H. Matsui on Nov., 2008
!
!!      subroutine const_FEM_3d_filtering_tables                        &
!!     &         (SGS_par, mesh, filtering, wide_filtering)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(filtering_data_type), intent(inout)  :: filtering
!!        type(filtering_data_type), intent(inout)  :: wide_filtering
!
      module t_FEM_MHD_filter_data
!
      use m_precision
      use t_layering_ele_list
      use t_filter_elength
      use t_filtering_data
!
      implicit none
!
!
!>      Structure of filtering data for FEM
      type filters_on_FEM
!>        Structure of grouping of elements
        type(layering_tbl) :: layer_tbl
!
!>        Structure of element size for nonlinear gradient model
        type(gradient_model_data_type) :: FEM_elens
!
!>        Weights for filtering
        type(filtering_data_type) :: filtering
!>        Weights for wide filtering
        type(filtering_data_type) :: wide_filtering
      end type filters_on_FEM
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine const_FEM_3d_filtering_tables                          &
     &         (SGS_par, mesh, filtering, wide_filtering)
!
      use t_mesh_data
      use t_SGS_control_parameter
      use ordering_line_filter_smp
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
!
      type(filtering_data_type), intent(inout)  :: filtering
      type(filtering_data_type), intent(inout)  :: wide_filtering
!
      integer(kind = kint) :: iflag
!
!
      iflag = SGS_par%filter_p%iflag_SGS_filter
      if(      (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF   &
     &     .or. SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity)) then
!
        if   (iflag .eq. id_SGS_3D_FILTERING                            &
     &   .or. iflag .eq. id_SGS_3D_EZ_FILTERING) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*)' s_set_istart_3d_filtering'
          call s_set_istart_3d_filtering(filtering%filter)
!
        else if (iflag.eq.id_SGS_3D_SMP_FILTERING                       &
     &     .or. iflag.eq.id_SGS_3D_EZ_SMP_FILTERING) then
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*) ' const_tbl_3d_filtering_smp'
          call const_tbl_3d_filtering_smp(filtering)
!
        else if (iflag .eq. id_SGS_LINE_FILTERING) then
          if (iflag_debug.gt.0) write(*,*)' ordering_l_filter_smp'
          call ordering_l_filter_smp(mesh%node%istack_nod_smp,          &
     &        filtering%fil_l, filtering%fil_l_smp)
        end if
      end if
!
      if(      (SGS_par%model_p%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF   &
     &    .and. SGS_par%model_p%iflag_SGS.eq.id_SGS_similarity)) then
        if    (iflag .eq. id_SGS_3D_FILTERING                           &
     &    .or. iflag .eq. id_SGS_3D_EZ_FILTERING) then
          if (iflag_debug .gt. 0) write(*,*)' s_set_istart_w_filtering'
          call s_set_istart_3d_filtering(wide_filtering%filter)
!
        else if (iflag.eq.id_SGS_3D_SMP_FILTERING                       &
     &     .or. iflag.eq.id_SGS_3D_EZ_SMP_FILTERING) then
!
          if (iflag_debug .gt. 0)                                       &
     &      write(*,*) 'const_tbl_3d_filtering_smp'
          call const_tbl_3d_filtering_smp(wide_filtering)
        end if
      end if
!
      end subroutine const_FEM_3d_filtering_tables
!
! ----------------------------------------------------------------------
!
      end module t_FEM_MHD_filter_data
