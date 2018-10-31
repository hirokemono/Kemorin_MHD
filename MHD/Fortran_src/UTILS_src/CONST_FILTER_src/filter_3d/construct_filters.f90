!
!      module construct_filters
!
!     Written by H. Matsui on Mar., 2008
!
!!      subroutine select_const_filter(file_name, mesh, fem_int,        &
!!     &          tbl_crs, rhs_mat, FEM_elen, dxidxs, FEM_moments)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
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
      use m_reference_moments
!
      use t_mesh_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_crs_connect
      use t_work_FEM_integration
      use t_filter_elength
      use t_filter_moments
      use t_filter_dxdxi
!
      use cal_element_size
      use cal_filter_moms_ele_by_elen
      use expand_filter_area_4_1node
      use binary_IO
!
      implicit none
!
      character(len=kchara), parameter, private :: tmp_head = 'work'
      type(file_IO_flags), private :: bin_flflags
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
      subroutine select_const_filter(file_name, mesh, fem_int,          &
     &          tbl_crs,  rhs_mat, FEM_elen, dxidxs, FEM_moments)
!
!
      character(len=kchara), intent(in) :: file_name
      type(mesh_geometry), intent(in) :: mesh
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
!
!
      if (iflag_tgt_filter_type .eq. 1)  then
        if (iflag_debug.eq.1) write(*,*) 'const_commutative_filter'
        call const_commutative_filter(file_name, mesh,                  &
     &     fem_int%jcs%g_FEM, fem_int%jcs%jac_3d, tbl_crs, rhs_mat,     &
     &     FEM_elen, FEM_moments)
!
      else if (iflag_tgt_filter_type .eq. -1) then
        if (iflag_debug.eq.1) write(*,*) 'correct_commutative_filter'
        call correct_commutative_filter                                 &
     &     (mesh, fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,                &
     &      tbl_crs, rhs_mat, FEM_elen, dxidxs, FEM_moments)
!
      else if (iflag_tgt_filter_type .ge. -4                            &
     &     .and. iflag_tgt_filter_type .le. -2) then
        if (iflag_debug.eq.1) write(*,*) 'correct_by_simple_filter'
        call correct_by_simple_filter                                   &
     &     (mesh, fem_int%jcs%g_FEM, fem_int%jcs%jac_3d,                &
     &      fem_int%rhs_tbl, fem_int%m_lump, tbl_crs, rhs_mat,          &
     &      FEM_elen, dxidxs, FEM_moments)
!
      else if (iflag_tgt_filter_type.ge.2                               &
     &     .and. iflag_tgt_filter_type.le.4) then
        if (iflag_debug.eq.1) write(*,*) 'const_simple_filter'
        call const_simple_filter(file_name, mesh, fem_int%jcs%g_FEM,    &
     &      fem_int%jcs%jac_3d, fem_int%rhs_tbl, fem_int%m_lump,        &
     &      tbl_crs, rhs_mat, FEM_elen, dxidxs, FEM_moments)
      end if
!
      end subroutine select_const_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_commutative_filter                               &
     &         (file_name, mesh, g_FEM, jac_3d_q,                       &
     &          tbl_crs, rhs_mat, FEM_elen, FEM_moments)
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
      if (itype_mass_matrix .eq. 1) then
        call release_mass_mat_for_consist(tbl_crs, rhs_mat)
      end if
!
!  ---------------------------------------------------
!     construct filter function
!  ---------------------------------------------------
!
      if(iflag_debug.eq.1)  write(*,*) 'const_commute_filter_coefs'
      call const_commute_filter_coefs(file_name, mesh,                  &
     &    g_FEM, jac_3d_q, FEM_elen, FEM_moments%mom_nod(1))
!
      if(iflag_debug.eq.1)  write(*,*)'const_fluid_filter_coefs'
      call const_fluid_filter_coefs                                     &
     &   (file_name, mesh, g_FEM, jac_3d_q, FEM_elen)
!
      call finalize_4_cal_fileters
!
      call deallocate_coef_4_filter_moms
!
      end subroutine const_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine const_simple_filter                                    &
     &         (file_name, mesh, g_FEM, jac_3d_q, rhs_tbl, m_lump,     &
     &          tbl_crs, rhs_mat, FEM_elen, dxidxs, FEM_moments)
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
      call set_simple_filter(file_name, mesh, g_FEM, jac_3d_q,          &
     &    FEM_elen, dxidxs, FEM_moments%mom_nod(1))
!
      if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
      call s_const_filter_mom_ele(mesh%nod_comm, mesh%node, mesh%ele,   &
     &    g_FEM, jac_3d_q, rhs_tbl, tbl_crs, m_lump, rhs_mat,           &
     &    FEM_moments%mom_nod(1), FEM_moments%mom_ele(1))
      if(iflag_debug.eq.1)                                              &
     &       write(*,*) 'cal_fmoms_ele_by_elen 2'
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(1))
!
      if(iflag_debug.eq.1)  write(*,*) 'set_simple_fluid_filter'
      call set_simple_fluid_filter(file_name, mesh,                     &
     &    g_FEM, jac_3d_q, FEM_elen, dxidxs, FEM_moments%mom_nod)
!
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
      if (itype_mass_matrix .eq. 1) then
        call release_mass_mat_for_consist(tbl_crs, rhs_mat)
      end if
!
      end subroutine const_simple_filter
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine correct_commutative_filter(mesh, g_FEM, jac_3d_q,      &
     &          tbl_crs, rhs_mat, FEM_elen, dxidxs, FEM_moments)
!
      use m_ctl_param_newdom_filter
      use m_filter_file_names
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use mesh_data_IO
      use mesh_data_IO_b
!
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
      type(mesh_geometry), intent(in) :: mesh
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
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
      if (itype_mass_matrix .eq. 1) then
        call release_mass_mat_for_consist(tbl_crs, rhs_mat)
      end if
!
!  ---------------------------------------------------
!     check filter function
!  ---------------------------------------------------
!
      file_name =       add_int_suffix(my_rank, org_filter_coef_head)
      fixed_file_name = add_int_suffix(my_rank, new_filter_coef_head)
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
        call open_read_binary_file                                      &
     &     (file_name, my_rank, bin_flflags%iflag_bin_swap)
        call read_filter_geometry_b                                     &
     &     (my_rank, bin_flflags, comm_IO, nod_IO)
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        call open_write_binary_file(fixed_file_name)
        call write_filter_geometry_b(my_rank, comm_IO, nod_IO)
        call close_binary_file
!
        call open_read_binary_file                                      &
     &     (file_name, my_rank, bin_flflags%iflag_bin_swap)
        call read_filter_geometry_b                                     &
     &     (my_rank, bin_flflags, comm_IO, nod_IO)
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) then
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
      call allocate_correct_filter_flag(mesh%node, mesh%ele)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters                                      &
     &   (org_filter_coef_code, fixed_file_name, mesh,                  &
     &    g_FEM, jac_3d_q, FEM_elen, dxidxs, FEM_moments%mom_nod(1),    &
     &    bin_flflags)
      if(bin_flflags%ierr_IO .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters                                  &
     &   (org_filter_coef_code, fixed_file_name, mesh, g_FEM, jac_3d_q, &
     &    FEM_elen, dxidxs, FEM_moments%mom_nod, bin_flflags)
      if(bin_flflags%ierr_IO .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        close(org_filter_coef_code)
      else
        call close_binary_file
      end if
!
      call deallocate_correct_filter_flag
!
      call deallocate_coef_4_filter_moms
      call finalize_4_cal_fileters
!
!
      end subroutine correct_commutative_filter
!
! ----------------------------------------------------------------------
!
      subroutine correct_by_simple_filter                               &
     &         (mesh, g_FEM, jac_3d_q, rhs_tbl, m_lump, tbl_crs,        &
     &          rhs_mat, FEM_elen, dxidxs, FEM_moments)
!
      use m_ctl_param_newdom_filter
      use m_filter_file_names
      use m_filter_coefs
      use m_field_file_format
      use set_parallel_file_name
      use correct_wrong_filters
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use mesh_data_IO
      use mesh_data_IO_b
!
      type(mesh_geometry), intent(in) :: mesh

      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(dxidx_data_type), intent(inout) :: dxidxs
      type(gradient_filter_mom_type), intent(inout) :: FEM_moments
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
      write(*,*) 'org_filter_coef_head', org_filter_coef_head
      file_name =        add_int_suffix(my_rank, org_filter_coef_head)
      fixed_file_name =  add_int_suffix(my_rank, new_filter_coef_head)
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
        call open_read_binary_file                                      &
     &     (file_name, my_rank, bin_flflags%iflag_bin_swap)
        call read_filter_geometry_b                                     &
     &     (my_rank, bin_flflags, comm_IO, nod_IO)
        call close_binary_file
        if(bin_flflags%ierr_IO .gt. 0) then
          call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
        end if
!
        call open_write_binary_file(fixed_file_name)
        call write_filter_geometry_b(my_rank, comm_IO, nod_IO)
        call close_binary_file
!
        call open_read_binary_file                                      &
     &     (file_name, my_rank, bin_flflags%iflag_bin_swap)
        call read_filter_geometry_b                                     &
     &     (my_rank, bin_flflags, comm_IO, nod_IO)
        if(bin_flflags%ierr_IO .gt. 0) then
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
      call allocate_correct_filter_flag(mesh%node, mesh%ele)
!
      if(iflag_debug.eq.1)  write(*,*) 's_correct_wrong_filters'
      call s_correct_wrong_filters                                      &
     &   (org_filter_coef_code, fixed_file_name, mesh,                  &
     &    g_FEM, jac_3d_q, FEM_elen, dxidxs, FEM_moments%mom_nod(1),    &
     &    bin_flflags)
      if(bin_flflags%ierr_IO .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
      if(iflag_debug.eq.1)  write(*,*) 's_const_filter_mom_ele 1'
      call s_const_filter_mom_ele(mesh%nod_comm, mesh%node, mesh%ele,   &
     &    g_FEM, jac_3d_q, rhs_tbl, tbl_crs, m_lump, rhs_mat,          &
     &    FEM_moments%mom_nod(1), FEM_moments%mom_ele(1))
      if(iflag_debug.eq.1)                                              &
     &       write(*,*) 'correct_fmoms_ele_by_elen 1'
      call correct_fmoms_ele_by_elen                                    &
     &   (mesh%ele, FEM_elen, FEM_moments%mom_ele(1))
        if(iflag_debug.eq.1)                                            &
     &       write(*,*) 'cal_fmoms_ele_by_elen 2'
      call cal_fmoms_ele_by_elen(FEM_elen, FEM_moments%mom_ele(2))
!
      if(iflag_debug.eq.1)  write(*,*)'correct_wrong_fluid_filters'
      call correct_wrong_fluid_filters                                  &
     &   (org_filter_coef_code, fixed_file_name, mesh, g_FEM, jac_3d_q, &
     &    FEM_elen, dxidxs, FEM_moments%mom_nod, bin_flflags)
      if(bin_flflags%ierr_IO .gt. 0) then
        call calypso_mpi_abort(ierr, 'Filter data is wrong!!')
      end if
!
      if (ifmt_3d_filter .eq. iflag_ascii) then
        close(org_filter_coef_code)
      else
        call close_binary_file
      end if
!
      call deallocate_correct_filter_flag
!
      call deallocate_coef_4_filter_moms
      call finalize_4_cal_fileters
!
!
      end subroutine correct_by_simple_filter
!
! ----------------------------------------------------------------------
!
      end module construct_filters
