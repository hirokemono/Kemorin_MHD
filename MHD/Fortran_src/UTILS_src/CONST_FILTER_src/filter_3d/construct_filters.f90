!
!      module construct_filters
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine select_const_filter(file_name, newfil_p, mesh,       &
!!     &         fem_int, tbl_crs, rhs_mat, FEM_elen, fil_elist, gfil_p,&
!!     &         ref_m, dxidxs, FEM_moments, fil_gen, f_matrices, v_sol)
!!        type(ctl_param_newdom_filter), intent(in) :: newfil_p
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(element_list_4_filter), intent(in) :: fil_elist
!!        type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
!!        type(reference_moments), intent(inout) :: ref_m
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(dxidx_data_type), intent(inout) :: dxidxs
!!        type(gradient_filter_mom_type), intent(inout) :: FEM_moments
!!        type(const_filter_coefs), intent(inout) :: fil_gen
!!        type(vectors_4_solver), intent(inout) :: v_sol
!
      module construct_filters
!
      use m_precision
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_mesh_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_crs_connect
      use t_work_FEM_integration
      use t_filter_elength
      use t_filter_moments
      use t_filter_dxdxi
      use t_reference_moments
      use t_filter_coefs
      use t_matrix_4_filter
      use t_element_list_4_filter
      use t_ctl_params_4_gen_filter
      use t_ctl_param_newdom_filter
      use t_binary_IO_buffer
      use t_vector_for_solver
!
      use cal_element_size
      use cal_filter_moms_ele_by_elen
      use expand_filter_area_4_1node
      use binary_IO
!
      implicit none
!
      character(len=kchara), parameter, private :: tmp_head = 'work'
      integer(kind = kint), parameter :: id_read_org =  21
      integer(kind = kint), parameter :: id_read_new =  31
      integer(kind = kint), parameter :: id_write_new = 32
      type(binary_IO_buffer) :: bbuf_org
      type(binary_IO_buffer) :: bbuf_new
!
      integer(kind = kint), parameter, private                          &
     &                                :: id_new_filter_coef = 33
!
      private :: id_read_org, id_read_new, id_write_new
      private :: bbuf_org, bbuf_new
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
      subroutine select_const_filter(file_name, newfil_p, mesh,         &
     &         fem_int, tbl_crs, rhs_mat, FEM_elen, fil_elist, gfil_p,  &
     &         ref_m, dxidxs, FEM_moments, fil_gen, f_matrices, v_sol)
!
      character(len=kchara), intent(in) :: file_name
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
      type(mesh_geometry), intent(in) :: mesh
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(reference_moments), intent(inout) :: ref_m
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
      type(const_filter_coefs), intent(inout) :: fil_gen
      type(matrices_4_filter), intent(inout) :: f_matrices
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call reset_failed_filter_flag(fil_gen%whole_area)
      call reset_failed_filter_flag(fil_gen%fluid_area)
!
      if (gfil_p%iflag_tgt_filter_type .eq. iflag_commutative)  then
        if (iflag_debug.eq.1) write(*,*) 'const_commutative_filter'
        call const_commutative_filter                                   &
     &     (file_name, mesh, fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,     &
     &      tbl_crs, rhs_mat, FEM_elen, ref_m, fil_elist, gfil_p,       &
     &      FEM_moments, fil_gen, f_matrices)
        call finalize_4_cal_fileters(gfil_p%iflag_ordering_list,        &
     &     fil_gen%fil_coef, f_matrices%fil_tbl_crs,                    &
     &     f_matrices%fil_mat_crs, f_matrices%fil_mat, ref_m)
!
      else if(gfil_p%iflag_tgt_filter_type                              &
     &                    .eq. -iflag_commutative) then
        if (iflag_debug.eq.1) write(*,*) 'correct_commutative_filter'
        call correct_commutative_filter(newfil_p, mesh,                 &
     &      fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, tbl_crs,             &
     &      rhs_mat, FEM_elen, ref_m, fil_elist, gfil_p, dxidxs,        &
     &      FEM_moments, fil_gen, f_matrices)
        call finalize_4_cal_fileters(gfil_p%iflag_ordering_list,        &
     &     fil_gen%fil_coef, f_matrices%fil_tbl_crs,                    &
     &     f_matrices%fil_mat_crs, f_matrices%fil_mat, ref_m)
!
      else if (gfil_p%iflag_tgt_filter_type .ge. -iflag_gaussian        &
     &     .and. gfil_p%iflag_tgt_filter_type .le. -iflag_tophat) then
        if (iflag_debug.eq.1) write(*,*) 'correct_by_simple_filter'
        call correct_by_simple_filter                                   &
     &     (mesh, newfil_p, fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,      &
     &      fem_int%rhs_tbl, fem_int%m_lump, tbl_crs, rhs_mat,          &
     &      FEM_elen, ref_m, fil_elist, gfil_p, dxidxs, FEM_moments,    &
     &      fil_gen, f_matrices, v_sol)
        call finalize_4_cal_fileters(gfil_p%iflag_ordering_list,        &
     &     fil_gen%fil_coef, f_matrices%fil_tbl_crs,                    &
     &      f_matrices%fil_mat_crs, f_matrices%fil_mat, ref_m)
!
      else if (gfil_p%iflag_tgt_filter_type.ge.iflag_tophat             &
     &     .and. gfil_p%iflag_tgt_filter_type.le.iflag_gaussian) then
        if (iflag_debug.eq.1) write(*,*) 'const_simple_filter'
        call const_simple_filter                                        &
     &     (file_name, mesh, fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,     &
     &      fem_int%rhs_tbl, fem_int%m_lump, tbl_crs, rhs_mat,          &
     &      FEM_elen, ref_m, fil_elist, gfil_p, dxidxs, FEM_moments,    &
     &      fil_gen%fil_coef, fil_gen%tmp_coef, f_matrices, v_sol)
      end if
!
      end subroutine select_const_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_commutative_filter                               &
     &         (file_name, mesh, g_FEM, jac_3d_q, tbl_crs, rhs_mat,     &
     &          FEM_elen, ref_m, fil_elist, gfil_p, FEM_moments,        &
     &          fil_gen, f_matrices)
!
      use cal_filter_func_node
!
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
      type(const_filter_coefs), intent(inout) :: fil_gen
      type(matrices_4_filter), intent(inout) :: f_matrices
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
      if(gfil_p%itype_mass_matrix .eq. 1) then
        call release_mass_mat_for_consist(tbl_crs, rhs_mat)
      end if
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
      if(iflag_debug.eq.1)  write(*,*) 'const_commute_filter_coefs'
      call const_commute_filter_coefs(file_name, mesh, g_FEM, jac_3d_q, &
     &    FEM_elen, ref_m, FEM_moments%mom_nod(1), gfil_p,              &
     &    fil_gen%fil_coef, fil_gen%tmp_coef,                           &
     &    fil_gen%whole_area, f_matrices)
!
      if(iflag_debug.eq.1)  write(*,*)'const_fluid_filter_coefs'
      call const_fluid_filter_coefs(file_name, gfil_p, mesh,            &
     &    g_FEM, jac_3d_q, FEM_elen, ref_m, fil_elist,                  &
     &    fil_gen%fil_coef, fil_gen%tmp_coef, fil_gen%fluid_area,       &
     &    f_matrices)
!
      end subroutine const_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_simple_filter                                    &
     &         (file_name, mesh, g_FEM, jac_3d_q, rhs_tbl, m_lump,      &
     &          tbl_crs, rhs_mat, FEM_elen, ref_m, fil_elist, gfil_p,   &
     &          dxidxs, FEM_moments, fil_coef, tmp_coef,                &
     &          f_matrices, v_sol)
!
      use cal_1st_diff_deltax_4_nod
      use cal_filter_func_node
      use cal_diff_elesize_on_ele
      use filter_coefs_file_IO
!
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
      type(each_filter_coef), intent(inout) :: fil_coef, tmp_coef
      type(matrices_4_filter), intent(inout) :: f_matrices
      type(vectors_4_solver), intent(inout) :: v_sol
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
      call set_simple_filter(file_name, mesh, g_FEM, jac_3d_q,          &
     &    FEM_elen, ref_m, gfil_p, dxidxs, FEM_moments%mom_nod(1),      &
     &    fil_coef, tmp_coef, f_matrices)
!
      if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
      call s_const_filter_mom_ele(mesh%nod_comm, mesh%node, mesh%ele,   &
     &    g_FEM, jac_3d_q, rhs_tbl, tbl_crs, m_lump, rhs_mat, gfil_p,   &
     &    FEM_moments%mom_nod(1), FEM_moments%mom_ele(1), v_sol)
      if(iflag_debug.eq.1)                                              &
     &       write(*,*) 'cal_fmoms_ele_by_elen 2'
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(1))
!
      if(iflag_debug.eq.1)  write(*,*) 'set_simple_fluid_filter'
      call set_simple_fluid_filter                                      &
     &   (file_name, gfil_p, mesh, g_FEM, jac_3d_q,                     &
     &    FEM_elen, ref_m, fil_elist, dxidxs, FEM_moments%mom_nod,      &
     &    fil_coef, f_matrices)
!
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
      if(gfil_p%itype_mass_matrix .eq. 1) then
        call release_mass_mat_for_consist(tbl_crs, rhs_mat)
      end if
!
      end subroutine const_simple_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine correct_commutative_filter                             &
     &         (newfil_p, mesh, g_FEM, jac_3d_q, tbl_crs, rhs_mat,      &
     &          FEM_elen, ref_m, fil_elist, gfil_p, dxidxs,             &
     &          FEM_moments, fil_gen, f_matrices)
!
      use m_filter_file_names
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use mesh_data_IO
      use mesh_data_IO_b
!
      type(ctl_param_newdom_filter), intent(in) :: newfil_p
      type(mesh_geometry), intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
      type(const_filter_coefs), intent(inout) :: fil_gen
      type(matrices_4_filter), intent(inout) :: f_matrices
!
      type(communication_table) :: comm_IO
      type(node_data) ::           nod_IO
      character(len=kchara) :: file_name, fixed_file_name
      integer(kind = kint) :: ierr
!
!
      if(iflag_debug.eq.1)  write(*,*)'cal_fmoms_ele_by_elen'
      call alloc_filter_moms_nod_type                                   &
     &   (FEM_elen%nnod_filter_mom, FEM_moments)
      call alloc_filter_moms_ele_type                                   &
     &   (FEM_elen%nele_filter_mom, FEM_moments)
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(1))
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
      if(gfil_p%itype_mass_matrix .eq. 1) then
        call release_mass_mat_for_consist(tbl_crs, rhs_mat)
      end if
!
!  ---------------------------------------------------
!     check filter function
!  ---------------------------------------------------
!
      file_name                                                         &
     &      = add_process_id(my_rank, newfil_p%org_filter_coef_head)
      fixed_file_name                                                   &
     &      = add_process_id(my_rank, newfil_p%new_filter_coef_head)
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry                                       &
     &     (org_filter_coef_code, my_rank, comm_IO, nod_IO, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        open(id_new_filter_coef, file=fixed_file_name,                  &
     &       form='formatted')
        call write_filter_geometry                                      &
     &     (id_new_filter_coef, my_rank, comm_IO, nod_IO)
        close(id_new_filter_coef)
      else
        bbuf_new%id_binary = id_read_new
        call open_read_binary_file(file_name, my_rank, bbuf_new)
        if(bbuf_new%ierr_bin .ne. 0) goto 99
        call read_filter_geometry_b                                     &
     &     (my_rank, bbuf_new, comm_IO, nod_IO)
!
  99    continue
        call close_binary_file(bbuf_new)
!
        if(bbuf_new%ierr_bin .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        bbuf_new%id_binary = id_write_new
        call open_write_binary_file(fixed_file_name, bbuf_new)
        if(bbuf_new%ierr_bin .gt. 0) go to 98
        call write_filter_geometry_b                                    &
     &     (my_rank, comm_IO, nod_IO, bbuf_new)
!
  98    continue
        call close_binary_file(bbuf_new)
!
        bbuf_org%id_binary = id_read_org
        call open_read_binary_file(file_name, my_rank, bbuf_org)
        if(bbuf_org%ierr_bin .ne. 0) goto 97
        call read_filter_geometry_b                                     &
     &     (my_rank, bbuf_org, comm_IO, nod_IO)
!
        if(bbuf_org%ierr_bin .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        call dealloc_node_geometry_base(nod_IO)
        call dealloc_comm_table(comm_IO)
      end if
!
!
!  ---------------------------------------------------
!     correct filter function
!  ---------------------------------------------------
!
      call alloc_correct_filter_flag(mesh%node, fil_gen%whole_area)
      call alloc_correct_filter_flag(mesh%node, fil_gen%fluid_area)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters                                      &
     &   (org_filter_coef_code, fixed_file_name, mesh,                  &
     &    g_FEM, jac_3d_q, FEM_elen, ref_m, gfil_p, dxidxs,             &
     &    FEM_moments%mom_nod(1), fil_gen%fil_coef, fil_gen%tmp_coef,   &
     &    fil_gen%whole_area, fil_gen%fluid_area, f_matrices, bbuf_org)
      if(bbuf_org%ierr_bin .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters                                  &
     &   (org_filter_coef_code, fixed_file_name, gfil_p, mesh,          &
     &    g_FEM, jac_3d_q, FEM_elen, ref_m, fil_elist, dxidxs,          &
     &    FEM_moments%mom_nod, fil_gen%fil_coef, fil_gen%tmp_coef,      &
     &    fil_gen%fluid_area, f_matrices, bbuf_org)
      if(bbuf_org%ierr_bin .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
  97  continue
      if (ifmt_3d_filter .eq. iflag_ascii) then
        close(org_filter_coef_code)
      else
        call close_binary_file(bbuf_org)
      end if
!
      call dealloc_correct_filter_flag(fil_gen%whole_area)
      call dealloc_correct_filter_flag(fil_gen%fluid_area)
!
      end subroutine correct_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine correct_by_simple_filter(mesh, newfil_p,               &
     &          g_FEM, jac_3d_q, rhs_tbl, m_lump, tbl_crs,              &
     &          rhs_mat, FEM_elen, ref_m, fil_elist, gfil_p, dxidxs,    &
     &          FEM_moments, fil_gen, f_matrices, v_sol)
!
      use m_filter_file_names
      use m_field_file_format
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use mesh_data_IO
      use mesh_data_IO_b
!
      type(mesh_geometry), intent(in) :: mesh
      type(ctl_param_newdom_filter), intent(in) :: newfil_p

      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(reference_moments), intent(in) :: ref_m
      type(element_list_4_filter), intent(in) :: fil_elist
!
      type(ctl_params_4_gen_filter), intent(inout) :: gfil_p
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
      type(const_filter_coefs), intent(inout) :: fil_gen
      type(matrices_4_filter), intent(inout) :: f_matrices
      type(vectors_4_solver), intent(inout) :: v_sol
!
      type(communication_table) :: comm_IO
      type(node_data) ::           nod_IO
      character(len=kchara) :: file_name, fixed_file_name
      integer(kind = kint) :: ierr
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
      file_name                                                         &
     &      = add_process_id(my_rank, newfil_p%org_filter_coef_head)
      fixed_file_name                                                   &
     &      = add_process_id(my_rank, newfil_p%new_filter_coef_head)
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        open(org_filter_coef_code, file=file_name, form='formatted')
        call read_filter_geometry                                       &
     &     (org_filter_coef_code, my_rank, comm_IO, nod_IO, ierr)
        if(ierr .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        open(id_new_filter_coef, file=fixed_file_name,                  &
     &       form='formatted')
        call write_filter_geometry                                      &
     &     (id_new_filter_coef, my_rank, comm_IO, nod_IO)
        close(id_new_filter_coef)
      else
        bbuf_new%id_binary = id_read_new
        call open_read_binary_file(file_name, my_rank, bbuf_new)
        if(bbuf_new%ierr_bin .ne. 0) goto 99
        call read_filter_geometry_b                                     &
     &     (my_rank, bbuf_new, comm_IO, nod_IO)
!
  99    continue
        call close_binary_file(bbuf_new)
!
        if(bbuf_new%ierr_bin .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        bbuf_new%id_binary = id_write_new
        call open_write_binary_file(fixed_file_name, bbuf_new)
        if(bbuf_new%ierr_bin .gt. 0) go to 98
        call write_filter_geometry_b                                    &
     &     (my_rank, comm_IO, nod_IO, bbuf_new)
!
  98    continue
        call close_binary_file(bbuf_new)
!
        bbuf_org%id_binary = id_read_org
        call open_read_binary_file(file_name, my_rank, bbuf_org)
        if(bbuf_org%ierr_bin .ne. 0) goto 97
        call read_filter_geometry_b                                     &
     &     (my_rank, bbuf_org, comm_IO, nod_IO)

        if(bbuf_org%ierr_bin .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        call dealloc_node_geometry_base(nod_IO)
        call dealloc_comm_table(comm_IO)
      end if
!
!  ---------------------------------------------------
!     correct filter function
!  ---------------------------------------------------
!
      call alloc_correct_filter_flag(mesh%node, fil_gen%whole_area)
      call alloc_correct_filter_flag(mesh%node, fil_gen%fluid_area)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters                                      &
     &   (org_filter_coef_code, fixed_file_name, mesh,                  &
     &    g_FEM, jac_3d_q, FEM_elen, ref_m, gfil_p, dxidxs,             &
     &    FEM_moments%mom_nod(1), fil_gen%fil_coef, fil_gen%tmp_coef,   &
     &    fil_gen%whole_area, fil_gen%fluid_area, f_matrices, bbuf_org)
      if(bbuf_org%ierr_bin .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
      if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
      call s_const_filter_mom_ele(mesh%nod_comm, mesh%node, mesh%ele,   &
     &    g_FEM, jac_3d_q, rhs_tbl, tbl_crs, m_lump, rhs_mat, gfil_p,   &
     &    FEM_moments%mom_nod(1), FEM_moments%mom_ele(1), v_sol)
      if(iflag_debug.eq.1)                                              &
     &       write(*,*) 'correct_fmoms_ele_by_elen 1'
      call correct_fmoms_ele_by_elen                                    &
     &   (mesh%ele, fil_gen%whole_area, fil_gen%fluid_area,             &
     &    FEM_elen, FEM_moments%mom_ele(1))
        if(iflag_debug.eq.1)                                            &
     &       write(*,*) 'cal_fmoms_ele_by_elen 2'
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters                                  &
     &   (org_filter_coef_code, fixed_file_name, gfil_p, mesh,          &
     &    g_FEM, jac_3d_q, FEM_elen, ref_m, fil_elist, dxidxs,          &
     &    FEM_moments%mom_nod, fil_gen%fil_coef, fil_gen%tmp_coef,      &
     &    fil_gen%fluid_area, f_matrices, bbuf_org)
      if(bbuf_org%ierr_bin .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
  97  continue
      if (ifmt_3d_filter .eq. iflag_ascii) then
        close(org_filter_coef_code)
      else
        call close_binary_file(bbuf_org)
      end if
!
      call dealloc_correct_filter_flag(fil_gen%whole_area)
      call dealloc_correct_filter_flag(fil_gen%fluid_area)
!
      end subroutine correct_by_simple_filter
!
! ----------------------------------------------------------------------
!
      end module construct_filters
