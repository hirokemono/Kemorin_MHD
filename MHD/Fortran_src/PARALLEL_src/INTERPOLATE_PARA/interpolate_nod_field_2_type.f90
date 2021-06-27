!>@file   interpolate_nod_field_2_type
!!@brief  module interpolate_nod_field_2_type
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  interpolation for nodal field using module
!!
!!@verbatim
!!      subroutine init_interpolate_nodal_data                          &
!!     &         (node_org, ele_org, nod_dest, itp_table,               &
!!     &          v_1st_sol, v_2nd_sol)
!!      subroutine interpolate_nodal_data(node_org, fld_org,            &
!!     &          comm_dest, itp_table, nod_dest, phys_dest,            &
!!     &          v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!!        integer(kind = kint), intent(in) :: numdir, i_dest, i_origin
!!        type(node_data), intent(in) :: node_org
!!        type(element_data), intent(in) :: ele_org
!!        type(phys_data),intent(in) :: fld_org
!!        type(node_data), intent(in) :: nod_dest
!!        type(communication_table), intent(in) :: comm_dest
!!        type(phys_data), intent(inout) :: phys_dest
!!        type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module interpolate_nod_field_2_type
!
      use m_precision
      use m_machine_parameter
!
      use m_phys_constants
      use t_geometry_data
      use t_comm_table
      use t_phys_data
      use t_interpolate_table
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
      private :: s_interpolate_scalar, s_interpolate_vector
      private :: s_interpolate_tensor, s_interpolate_fields
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_interpolate_nodal_data                            &
     &         (node_org, ele_org, nod_dest, itp_table,                 &
     &          v_1st_sol, v_2nd_sol)
!
      type(node_data), intent(in) :: node_org
      type(element_data), intent(in) :: ele_org
      type(node_data), intent(in) :: nod_dest
!
      type(interpolate_table), intent(inout) :: itp_table
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
!
!
      if (iflag_debug.eq.1) write(*,*) 'const_interporate_matrix'
      call const_interporate_matrix                                     &
     &   (ele_org, itp_table%tbl_org, itp_table%mat)
!
      call verify_iccgN_vec_type                                        &
     &   (n_sym_tensor, node_org%numnod, v_1st_sol)
      call verify_iccgN_vec_type                                        &
     &   (n_sym_tensor, nod_dest%numnod, v_2nd_sol)
!
      end subroutine init_interpolate_nodal_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine interpolate_nodal_data(node_org, fld_org,              &
     &          comm_dest, itp_table, nod_dest, phys_dest,              &
     &          v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!
      use calypso_mpi
!
      integer(kind = kint) :: i, i_dest, i_origin
      type(node_data), intent(in) :: node_org
      type(phys_data), intent(in) :: fld_org
!
      type(node_data), intent(in) :: nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
!
      type(phys_data), intent(inout) :: phys_dest
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      do i = 1, fld_org%num_phys
        i_origin = fld_org%istack_component(i-1) + 1
        i_dest =   phys_dest%istack_component(i-1) + 1
        if      (fld_org%num_component(i) .eq. n_scalar) then
          if (my_rank.eq.0) write(*,*) ' interpolate scalar: ',         &
     &            trim(fld_org%phys_name(i)), '  ', i_dest, i_origin
          call s_interpolate_scalar(i_dest, i_origin,                   &
     &        node_org, fld_org, comm_dest, itp_table, nod_dest,        &
     &        phys_dest, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!
        else if (fld_org%num_component(i) .eq. n_vector) then
          if (my_rank.eq.0) write(*,*) ' interpolate vector: ',         &
     &            trim(fld_org%phys_name(i)), '  ', i_dest, i_origin
          call s_interpolate_vector(i_dest, i_origin,                   &
     &        node_org, fld_org, comm_dest, itp_table,                  &
     &        nod_dest, phys_dest, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
!
        else if (fld_org%num_component(i) .eq. n_sym_tensor) then
          if (my_rank.eq.0) write(*,*) ' interpolate tensor: ',         &
     &            trim(fld_org%phys_name(i)), '  ', i_dest, i_origin
          call s_interpolate_tensor(i_dest, i_origin,                   &
     &        node_org, fld_org, comm_dest, itp_table,                  &
     &        nod_dest, phys_dest, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
        else
          if (my_rank.eq.0) write(*,*) ' interpolate tensor: ',         &
     &            trim(fld_org%phys_name(i)), '  ', i_dest, i_origin
          call s_interpolate_fields                                     &
     &       (fld_org%num_component(i), i_dest, i_origin,               &
     &        node_org, fld_org, comm_dest, itp_table,                  &
     &        nod_dest, phys_dest, v_1st_sol, v_2nd_sol, SR_sig, SR_r)
        end if
      end do
!
      end subroutine interpolate_nodal_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_scalar                                   &
     &         (i_dest, i_origin, node_org, fld_org, comm_dest,         &
     &          itp_table, nod_dest, phys_dest, v_1st_sol, v_2nd_sol,   &
     &          SR_sig, SR_r)
!
      use interpolate_by_module
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
!
      type(node_data), intent(in) :: node_org, nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      type(phys_data), intent(in) :: fld_org
!
      type(phys_data), intent(inout) :: phys_dest
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: inod
!
!
      call verify_iccgN_vec_type(n_scalar, node_org%numnod, v_1st_sol)
      call verify_iccgN_vec_type(n_scalar, nod_dest%numnod, v_2nd_sol)
!
!$omp parallel do
      do inod = 1, node_org%numnod
        v_1st_sol%x_vec(inod) = fld_org%d_fld(inod,i_origin)
      end do
!$omp end parallel do
!
      call interpolate_mod_1(itp_table%iflag_itp_recv, comm_dest,       &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, node_org%numnod, nod_dest%numnod,                     &
     &    v_1st_sol%x_vec(1), SR_sig, SR_r, v_2nd_sol%x_vec(1))
!
!$omp parallel do
      do inod = 1, nod_dest%numnod
        phys_dest%d_fld(inod,i_dest) = v_2nd_sol%x_vec(inod)
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_scalar
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_vector(i_dest, i_origin,                 &
     &          node_org, fld_org, comm_dest, itp_table,                &
     &          nod_dest, phys_dest, v_1st_sol, v_2nd_sol,              &
     &          SR_sig, SR_r)
!
      use interpolate_by_module
!
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
!
      type(node_data), intent(in) :: node_org, nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      type(phys_data), intent(in) :: fld_org
!
      type(phys_data), intent(inout) :: phys_dest
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: inod
!
!     initialize
      call verify_iccgN_vec_type(n_vector, node_org%numnod, v_1st_sol)
      call verify_iccgN_vec_type(n_vector, nod_dest%numnod, v_2nd_sol)
!
!$omp parallel do
      do inod = 1, node_org%numnod
        v_1st_sol%x_vec(3*inod-2) = fld_org%d_fld(inod,i_origin  )
        v_1st_sol%x_vec(3*inod-1) = fld_org%d_fld(inod,i_origin+1)
        v_1st_sol%x_vec(3*inod  ) = fld_org%d_fld(inod,i_origin+2)
      end do
!$omp end parallel do
!
!    interpolation
!
      call interpolate_mod_3(itp_table%iflag_itp_recv, comm_dest,       &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, node_org%numnod, nod_dest%numnod,                     &
     &    v_1st_sol%x_vec(1), SR_sig, SR_r, v_2nd_sol%x_vec(1))
!
!$omp parallel do
      do inod = 1, nod_dest%numnod
        phys_dest%d_fld(inod,i_dest  ) = v_2nd_sol%x_vec(3*inod-2)
        phys_dest%d_fld(inod,i_dest+1) = v_2nd_sol%x_vec(3*inod-1)
        phys_dest%d_fld(inod,i_dest+2) = v_2nd_sol%x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_vector
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_tensor(i_dest, i_origin,                 &
     &          node_org, fld_org, comm_dest, itp_table,                &
     &          nod_dest, phys_dest, v_1st_sol, v_2nd_sol,              &
     &          SR_sig, SR_r)
!
      use interpolate_by_module
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
!
      type(node_data), intent(in) :: node_org, nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      type(phys_data), intent(in) :: fld_org
!
      type(phys_data), intent(inout) :: phys_dest
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: inod
!
!
      call verify_iccgN_vec_type                                        &
     &   (n_sym_tensor, node_org%numnod,  v_1st_sol)
      call verify_iccgN_vec_type                                        &
     &   (n_sym_tensor, nod_dest%numnod, v_2nd_sol)
!
!$omp parallel do
      do inod = 1, node_org%numnod
        v_1st_sol%x_vec(6*inod-5) = fld_org%d_fld(inod,i_origin  )
        v_1st_sol%x_vec(6*inod-4) = fld_org%d_fld(inod,i_origin+1)
        v_1st_sol%x_vec(6*inod-3) = fld_org%d_fld(inod,i_origin+2)
        v_1st_sol%x_vec(6*inod-2) = fld_org%d_fld(inod,i_origin+3)
        v_1st_sol%x_vec(6*inod-1) = fld_org%d_fld(inod,i_origin+4)
        v_1st_sol%x_vec(6*inod  ) = fld_org%d_fld(inod,i_origin+5)
      end do
!$omp end parallel do
!
!    interpolation
      call interpolate_mod_6(itp_table%iflag_itp_recv, comm_dest,       &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, node_org%numnod, nod_dest%numnod,                     &
     &    v_1st_sol%x_vec(1), SR_sig, SR_r, v_2nd_sol%x_vec(1))
!
!$omp parallel do
      do inod = 1, nod_dest%numnod
        phys_dest%d_fld(inod,i_dest  ) = v_2nd_sol%x_vec(6*inod-5)
        phys_dest%d_fld(inod,i_dest+1) = v_2nd_sol%x_vec(6*inod-4)
        phys_dest%d_fld(inod,i_dest+2) = v_2nd_sol%x_vec(6*inod-3)
        phys_dest%d_fld(inod,i_dest+3) = v_2nd_sol%x_vec(6*inod-2)
        phys_dest%d_fld(inod,i_dest+4) = v_2nd_sol%x_vec(6*inod-1)
        phys_dest%d_fld(inod,i_dest+5) = v_2nd_sol%x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_fields(numdir, i_dest, i_origin,         &
     &          node_org, fld_org, comm_dest, itp_table,                &
     &          nod_dest, phys_dest, v_1st_sol, v_2nd_sol,              &
     &          SR_sig, SR_r)
!
      use interpolate_by_module
!
      integer(kind = kint), intent(in) :: numdir, i_dest, i_origin
!
      type(node_data), intent(in) :: node_org, nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(interpolate_table), intent(in) :: itp_table
      type(phys_data), intent(in) :: fld_org
!
      type(phys_data), intent(inout) :: phys_dest
      type(vectors_4_solver), intent(inout) :: v_1st_sol, v_2nd_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: inod, nd
!
!     initialize
!
      call verify_iccgN_vec_type(numdir, node_org%numnod, v_1st_sol)
      call verify_iccgN_vec_type(numdir, nod_dest%numnod, v_2nd_sol)
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, node_org%numnod
          v_1st_sol%x_vec(numdir*(inod-1)+nd)                           &
     &          = fld_org%d_fld(inod,i_origin+nd-1)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!    interpolation
      call interpolate_mod_N(iflag_import_mod, comm_dest,               &
     &    itp_table%tbl_org, itp_table%tbl_dest, itp_table%mat,         &
     &    np_smp, node_org%numnod, nod_dest%numnod, numdir,             &
     &    v_1st_sol%x_vec(1), SR_sig, SR_r, v_2nd_sol%x_vec(1))
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, nod_dest%numnod
          phys_dest%d_fld(inod,i_dest+nd-1)                             &
     &         = v_2nd_sol%x_vec(numdir*(inod-1)+nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine s_interpolate_fields
!
! ----------------------------------------------------------------------
!
      end module interpolate_nod_field_2_type
