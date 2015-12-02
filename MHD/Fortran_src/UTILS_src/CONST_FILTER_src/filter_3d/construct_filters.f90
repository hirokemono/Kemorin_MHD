!
!      module construct_filters
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine select_const_filter(FEM_elen, dxidxs, FEM_moments)
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moments
!
      module construct_filters
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_gen_filter
      use m_geometry_data
      use m_jacobians
      use m_reference_moments
      use cal_element_size
!
      use t_filter_elength
      use t_filter_moments
      use t_filter_dxdxi
!
      use cal_filter_moms_ele_by_elen
      use expand_filter_area_4_1node
!
      implicit none
!
      character(len=kchara), parameter, private :: tmp_head = 'work'
!
      private :: const_commutative_filter, const_simple_filter
      private :: correct_commutative_filter, correct_by_simple_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine select_const_filter(FEM_elen, dxidxs, FEM_moments)
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
!
!
      if (iflag_tgt_filter_type .eq. 1)  then
        if (iflag_debug.eq.1) write(*,*) 'const_commutative_filter'
        call  const_commutative_filter(FEM_elen, FEM_moments)
!
      else if (iflag_tgt_filter_type .eq. -1) then
        if (iflag_debug.eq.1) write(*,*) 'correct_commutative_filter'
        call  correct_commutative_filter                                &
     &      (FEM_elen, dxidxs, FEM_moments)
!
      else if (iflag_tgt_filter_type .ge. -4                            &
     &     .and. iflag_tgt_filter_type .le. -2) then
        if (iflag_debug.eq.1) write(*,*) 'correct_by_simple_filter'
        call  correct_by_simple_filter(FEM_elen, dxidxs, FEM_moments)
!
      else if (iflag_tgt_filter_type.ge.2                               &
     &     .and. iflag_tgt_filter_type.le.4) then
        if (iflag_debug.eq.1) write(*,*) 'const_simple_filter'
        call const_simple_filter(FEM_elen, dxidxs, FEM_moments)
      end if
!
      end subroutine select_const_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_commutative_filter(FEM_elen, FEM_moments)
!
      use cal_filter_func_node
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
!
!
      if(iflag_debug.eq.1)  write(*,*)'cal_fmoms_ele_by_elen'
      call alloc_filter_moms_nod_type                                   &
     &   (FEM_elen%nnod_filter_mom, FEM_moments)
      call alloc_filter_moms_ele_type                                   &
     &   (FEM_elen%nele_filter_mom, FEM_moments)
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(1))
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
!
      if (itype_mass_matrix .eq. 1) call release_mass_mat_for_consist
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
      if(iflag_debug.eq.1)  write(*,*) 'const_commute_filter_coefs'
      call const_commute_filter_coefs(node1, ele1, jac1_3d_q,           &
     &    FEM_moments%mom_nod(1))
!
      if(iflag_debug.eq.1)  write(*,*)'const_fluid_filter_coefs'
      call const_fluid_filter_coefs(node1, ele1, jac1_3d_q)
!
      call finalize_4_cal_fileters
!
      call deallocate_coef_4_filter_moms
!
      end subroutine const_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_simple_filter(FEM_elen, dxidxs, FEM_moments)
!
      use m_finite_element_matrix
      use cal_1st_diff_deltax_4_nod
      use cal_filter_func_node
      use cal_diff_elesize_on_ele
      use filter_geometry_IO
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
!
!
      if(iflag_debug.eq.1) write(*,*) 'alloc_filter_moms_nod_type'
      call alloc_filter_moms_nod_type                                   &
     &   (FEM_elen%nnod_filter_mom, FEM_moments)
      if(iflag_debug.eq.1) write(*,*) 'alloc_filter_moms_ele_type'
      call alloc_filter_moms_ele_type                                   &
     &   (FEM_elen%nele_filter_mom, FEM_moments)
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
        if(iflag_debug.eq.1) write(*,*) 'set_simple_filter'
        call set_simple_filter(node1, ele1, jac1_3d_q,                  &
     &      dxidxs, FEM_moments%mom_nod(1))
!
          if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
        call s_const_filter_mom_ele                                     &
     &     (FEM_moments%mom_nod(1), FEM_moments%mom_ele(1))
          if(iflag_debug.eq.1)                                          &
     &       write(*,*) 'cal_fmoms_ele_by_elen 2'
        call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(1))
!
          if(iflag_debug.eq.1)  write(*,*) 'set_simple_fluid_filter'
        call set_simple_fluid_filter(node1, ele1, jac1_3d_q,            &
     &      dxidxs, FEM_moments%mom_nod)
!
        call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
        if (itype_mass_matrix .eq. 1) call release_mass_mat_for_consist
!
      end subroutine const_simple_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine correct_commutative_filter                             &
     &         (FEM_elen, dxidxs, FEM_moments)
!
      use m_filter_file_names
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_geometry_IO
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
      character(len=kchara) :: file_name
!
!
      if(iflag_debug.eq.1)  write(*,*)'cal_fmoms_ele_by_elen'
      call alloc_filter_moms_nod_type                                   &
     &   (FEM_elen%nnod_filter_mom, FEM_moments)
      call alloc_filter_moms_ele_type                                   &
     &   (FEM_elen%nele_filter_mom, FEM_moments)
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(1))
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
      if (itype_mass_matrix .eq. 1) call release_mass_mat_for_consist
!
!  ---------------------------------------------------
!     check filter function
!  ---------------------------------------------------
!
      call add_int_suffix(my_rank, org_filter_coef_head,                &
     &        file_name)
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry(org_filter_coef_code)
      else
        open(org_filter_coef_code, file=file_name, form='unformatted')
        call read_filter_geometry_b(org_filter_coef_code)
      end if
!
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
!
!  ---------------------------------------------------
!     correct filter function
!  ---------------------------------------------------
!
      call allocate_correct_filter_flag(node1%numnod, ele1%numele)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters(node1, ele1, jac1_3d_q,              &
     &    org_filter_coef_code, dxidxs, FEM_moments%mom_nod(1))
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters(node1, ele1, jac1_3d_q,          &
     &    org_filter_coef_code, dxidxs, FEM_moments%mom_nod)
!
      call deallocate_correct_filter_flag
!
      call deallocate_coef_4_filter_moms
      call finalize_4_cal_fileters
!
      close(org_filter_coef_code)
!
      end subroutine correct_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine correct_by_simple_filter                               &
     &         (FEM_elen, dxidxs, FEM_moments)
!
      use m_filter_file_names
      use m_filter_coefs
      use m_field_file_format
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_geometry_IO
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
      character(len=kchara) :: file_name
!
!
          if(iflag_debug.eq.1) write(*,*) 'alloc_filter_moms_nod_type'
      call alloc_filter_moms_nod_type                                   &
     &   (FEM_elen%nnod_filter_mom, FEM_moments)
          if(iflag_debug.eq.1) write(*,*) 'alloc_filter_moms_ele_type'
      call alloc_filter_moms_ele_type                                   &
     &   (FEM_elen%nele_filter_mom, FEM_moments)
!
!  ---------------------------------------------------
!     check filter function
!  ---------------------------------------------------
!
      write(*,*) 'org_filter_coef_head', org_filter_coef_head
      call add_int_suffix(my_rank, org_filter_coef_head, file_name)
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry(org_filter_coef_code)
      else
        open(org_filter_coef_code, file=file_name, form='unformatted')
        call read_filter_geometry_b(org_filter_coef_code)
      end if
!
      call deallocate_node_data_dummy
      call deallocate_comm_item_IO
!
!  ---------------------------------------------------
!     correct filter function
!  ---------------------------------------------------
!
      call allocate_correct_filter_flag(node1%numnod, ele1%numele)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters(node1, ele1, jac1_3d_q,              &
     &    org_filter_coef_code, dxidxs, FEM_moments%mom_nod(1))
!
        if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
      call s_const_filter_mom_ele                                       &
     &   (FEM_moments%mom_nod(1), FEM_moments%mom_ele(1))
        if(iflag_debug.eq.1)                                            &
     &       write(*,*) 'correct_fmoms_ele_by_elen 1'
      call correct_fmoms_ele_by_elen                                    &
     &   (ele1, FEM_elen, FEM_moments%mom_ele(1))
        if(iflag_debug.eq.1)                                            &
     &       write(*,*) 'cal_fmoms_ele_by_elen 2'
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters(node1, ele1, jac1_3d_q,          &
     &    org_filter_coef_code, dxidxs, FEM_moments%mom_nod)
!
      call deallocate_correct_filter_flag
!
      call deallocate_coef_4_filter_moms
      call finalize_4_cal_fileters
!
      close(org_filter_coef_code)
!
      end subroutine correct_by_simple_filter
!
! ----------------------------------------------------------------------
!
      end module construct_filters
