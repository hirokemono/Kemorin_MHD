!
!     module interpolate_field_type
!
!     Written by H. Matsui on Sep., 2006
!
!      subroutine interpolate_scalar_type(i_origin,i_dest,              &
!     &          itp_table, mesh_org, mesh_dst, fld_org, fld_dst)
!      subroutine interpolate_vector_type(i_origin, i_dest,             &
!     &          itp_table, mesh_org, mesh_dst, fld_org, fld_dst)
!      subroutine interpolate_tensor_type(i_origin, i_dest,             &
!     &          itp_table, mesh_org, mesh_dst, fld_org, fld_dst)
!
      module interpolate_field_type
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_phys_constants
      use m_array_for_send_recv
      use m_2nd_pallalel_vector
!
      use t_mesh_data
      use t_comm_table
      use t_interpolate_table
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_scalar_type(i_origin, i_dest,              &
     &          itp_table, mesh_org, mesh_dst, fld_org, fld_dst)
!
      use interpolate_by_module
!
!
      integer(kind = kint), intent(in) :: i_origin, i_dest
!
      type(mesh_geometry), intent(in) :: mesh_org
      type(mesh_geometry), intent(in) :: mesh_dst
      type(interpolate_table), intent(in) :: itp_table
!
      type(phys_data), intent(in) :: fld_org
      type(phys_data), intent(inout) :: fld_dst
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(n_scalar, mesh_org%node%numnod)
      call verify_2nd_iccg_matrix(n_scalar, mesh_dst%node%numnod)
!
!$omp parallel do
      do inod = 1, mesh_org%node%numnod
        x_vec(inod) = fld_org%d_fld(inod,i_origin  )
      end do
!$omp end parallel do
!
!    interpolation
      call interpolate_mod_1(mesh_dst%nod_comm,                         &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, mesh_org%node%numnod, mesh_dst%node%numnod,           &
     &    x_vec, xvec_2nd)
!
!$omp parallel do
      do inod = 1, mesh_dst%node%numnod
        fld_dst%d_fld(inod,i_dest  ) = xvec_2nd(inod)
      end do
!$omp end parallel do
!
      end subroutine interpolate_scalar_type
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_vector_type(i_origin, i_dest,              &
     &          itp_table, mesh_org, mesh_dst, fld_org, fld_dst)
!
      use interpolate_by_module
!
!
      integer(kind = kint), intent(in) :: i_origin, i_dest
!
      type(mesh_geometry), intent(in) :: mesh_org
      type(mesh_geometry), intent(in) :: mesh_dst
      type(interpolate_table), intent(in) :: itp_table
!
      type(phys_data), intent(in) :: fld_org
      type(phys_data), intent(inout) :: fld_dst
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(n_vector, mesh_org%node%numnod)
      call verify_2nd_iccg_matrix(n_vector, mesh_dst%node%numnod)
!
!$omp parallel do
      do inod = 1, mesh_org%node%numnod
        x_vec(3*inod-2) = fld_org%d_fld(inod,i_origin  )
        x_vec(3*inod-1) = fld_org%d_fld(inod,i_origin+1)
        x_vec(3*inod  ) = fld_org%d_fld(inod,i_origin+2)
      end do
!$omp end parallel do
!
!    interpolation
!
      call interpolate_mod_3(mesh_dst%nod_comm,                         &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, mesh_org%node%numnod, mesh_dst%node%numnod,           &
     &    x_vec, xvec_2nd)
!
!$omp parallel do
      do inod = 1, mesh_dst%node%numnod
        fld_dst%d_fld(inod,i_dest  ) = xvec_2nd(3*inod-2)
        fld_dst%d_fld(inod,i_dest+1) = xvec_2nd(3*inod-1)
        fld_dst%d_fld(inod,i_dest+2) = xvec_2nd(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine interpolate_vector_type
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_tensor_type(i_origin, i_dest,              &
     &          itp_table, mesh_org, mesh_dst, fld_org, fld_dst)
!
      use interpolate_by_module
!
!
      integer(kind = kint), intent(in) :: i_origin, i_dest
!
      type(mesh_geometry), intent(in) :: mesh_org
      type(mesh_geometry), intent(in) :: mesh_dst
      type(interpolate_table), intent(in) :: itp_table
!
      type(phys_data), intent(in) :: fld_org
      type(phys_data), intent(inout) :: fld_dst
!
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(n_sym_tensor, mesh_org%node%numnod)
      call verify_2nd_iccg_matrix(n_sym_tensor, mesh_dst%node%numnod)
!
!$omp parallel do
      do inod = 1, mesh_org%node%numnod
        x_vec(6*inod-5) = fld_org%d_fld(inod,i_origin  )
        x_vec(6*inod-4) = fld_org%d_fld(inod,i_origin+1)
        x_vec(6*inod-3) = fld_org%d_fld(inod,i_origin+2)
        x_vec(6*inod-2) = fld_org%d_fld(inod,i_origin+3)
        x_vec(6*inod-1) = fld_org%d_fld(inod,i_origin+4)
        x_vec(6*inod  ) = fld_org%d_fld(inod,i_origin+5)
      end do
!$omp end parallel do
!
!    interpolation
      call interpolate_mod_6(mesh_dst%nod_comm,                         &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, mesh_org%node%numnod, mesh_dst%node%numnod,           &
     &    x_vec, xvec_2nd)
!
!$omp parallel do
      do inod = 1, mesh_dst%node%numnod
        fld_dst%d_fld(inod,i_dest  ) = xvec_2nd(6*inod-5)
        fld_dst%d_fld(inod,i_dest+1) = xvec_2nd(6*inod-4)
        fld_dst%d_fld(inod,i_dest+2) = xvec_2nd(6*inod-3)
        fld_dst%d_fld(inod,i_dest+3) = xvec_2nd(6*inod-2)
        fld_dst%d_fld(inod,i_dest+4) = xvec_2nd(6*inod-1)
        fld_dst%d_fld(inod,i_dest+5) = xvec_2nd(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine interpolate_tensor_type
!
! ----------------------------------------------------------------------
!
      end module interpolate_field_type
