!>@file   interpolate_nod_field_2_type
!!@brief  module interpolate_nod_field_2_type
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  interpolation for nodal field using module
!!
!!@verbatim
!!      subroutine init_interpolate_nodal_data(nod_dest)
!!      subroutine interpolate_nodal_data(comm_dest, nod_dest, phys_dest)
!!
!!      subroutine s_interpolate_scalar(i_dest, i_origin,               &
!!     &          comm_dest, nod_dest, phys_dest)
!!      subroutine s_interpolate_vector(i_dest, i_origin,               &
!!     &          comm_dest, nod_dest, phys_dest)
!!      subroutine s_interpolate_tensor(i_dest, i_origin,               &
!!     &          comm_dest, nod_dest, phys_dest)
!!      subroutine s_interpolate_fields(numdir, i_dest, i_origin,       &
!!     &          comm_dest, nod_dest, phys_dest)
!!        integer(kind = kint), intent(in) :: numdir, i_dest, i_origin
!!        type(node_data), intent(in) :: nod_dest
!!        type(communication_table), intent(in) :: comm_dest
!!        type(phys_data), intent(inout) :: phys_dest
!!@endverbatim
!
      module interpolate_nod_field_2_type
!
      use m_precision
      use m_machine_parameter
!
      use m_phys_constants
      use m_node_phys_data
      use t_geometry_data
      use t_comm_table
      use t_phys_data
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
      subroutine init_interpolate_nodal_data(nod_dest)
!
      use m_array_for_send_recv
      use m_2nd_pallalel_vector
      use m_interpolate_table_orgin
      use m_interpolate_matrix
!
      type(node_data), intent(in) :: nod_dest
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_stack_tbl_wtype_org_smp'
      call set_stack_tbl_wtype_org_smp
!
      if (iflag_debug.eq.1) write(*,*) 'const_interporate_mat'
      call const_interporate_mat(numele, nnod_4_ele, ie)
!
      call verify_vector_for_solver(n_sym_tensor, node1%numnod)
      call verify_2nd_iccg_matrix(n_sym_tensor, nod_dest%numnod)
!
      end subroutine init_interpolate_nodal_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine interpolate_nodal_data(comm_dest, nod_dest, phys_dest)
!
      use calypso_mpi
!
      integer(kind = kint) :: i, i_dest, i_origin
      type(node_data), intent(in) :: nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(phys_data), intent(inout) :: phys_dest
!
!
      do i = 1, num_nod_phys
        i_origin = istack_nod_component(i-1) + 1
        i_dest =   phys_dest%istack_component(i-1) + 1
        if      (num_nod_component(i) .eq. n_scalar) then
          if (my_rank.eq.0) write(*,*) ' interpolate scalar: ',         &
     &            trim(phys_nod_name(i)), '  ', i_dest, i_origin
          call s_interpolate_scalar                                     &
     &       (i_dest, i_origin, comm_dest, nod_dest, phys_dest)
!
        else if (num_nod_component(i) .eq. n_vector) then
          if (my_rank.eq.0) write(*,*) ' interpolate vector: ',         &
     &            trim(phys_nod_name(i)), '  ', i_dest, i_origin
          call s_interpolate_vector                                     &
     &       (i_dest, i_origin, comm_dest, nod_dest, phys_dest)
!
        else if (num_nod_component(i) .eq. n_sym_tensor) then
          if (my_rank.eq.0) write(*,*) ' interpolate tensor: ',         &
     &            trim(phys_nod_name(i)), '  ', i_dest, i_origin
          call s_interpolate_tensor                                     &
     &       (i_dest, i_origin, comm_dest, nod_dest, phys_dest)
        else
          if (my_rank.eq.0) write(*,*) ' interpolate tensor: ',         &
     &            trim(phys_nod_name(i)), '  ', i_dest, i_origin
          call s_interpolate_fields(num_nod_component(i),               &
     &        i_dest, i_origin, comm_dest, nod_dest, phys_dest)
        end if
      end do
!
      end subroutine interpolate_nodal_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_scalar(i_dest, i_origin,                 &
     &          comm_dest, nod_dest, phys_dest)
!
      use m_2nd_pallalel_vector
      use interpolate_by_module
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
      type(node_data), intent(in) :: nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(phys_data), intent(inout) :: phys_dest
!
      integer(kind = kint) :: inod
!
!
      call verify_2nd_iccg_matrix(n_scalar, nod_dest%numnod)
!
      call interpolate_mod_1(comm_dest, node1%numnod, nod_dest%numnod,  &
     &    d_nod(1,i_origin), xvec_2nd(1))
!
!$omp parallel do
      do inod = 1, nod_dest%numnod
        phys_dest%d_fld(inod,i_dest) = xvec_2nd(inod)
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_scalar
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_vector(i_dest, i_origin,                 &
     &          comm_dest, nod_dest, phys_dest)
!
      use m_array_for_send_recv
      use m_2nd_pallalel_vector
      use interpolate_by_module
!
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
      type(node_data), intent(in) :: nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(phys_data), intent(inout) :: phys_dest
!
      integer(kind = kint) :: inod
!
!     initialize
      call verify_vector_for_solver(n_vector, node1%numnod)
      call verify_2nd_iccg_matrix(n_vector, nod_dest%numnod)
!
!$omp parallel do
      do inod = 1, node1%numnod
        x_vec(3*inod-2) = d_nod(inod,i_origin  )
        x_vec(3*inod-1) = d_nod(inod,i_origin+1)
        x_vec(3*inod  ) = d_nod(inod,i_origin+2)
      end do
!$omp end parallel do
!
!    interpolation
!
      call interpolate_mod_3(comm_dest, node1%numnod, nod_dest%numnod,  &
     &    x_vec(1), xvec_2nd(1))
!
!$omp parallel do
      do inod = 1, nod_dest%numnod
        phys_dest%d_fld(inod,i_dest  ) = xvec_2nd(3*inod-2)
        phys_dest%d_fld(inod,i_dest+1) = xvec_2nd(3*inod-1)
        phys_dest%d_fld(inod,i_dest+2) = xvec_2nd(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_vector
!
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_tensor(i_dest, i_origin,                 &
     &          comm_dest, nod_dest, phys_dest)
!
      use m_array_for_send_recv
      use m_2nd_pallalel_vector
      use interpolate_by_module
!
!
      integer(kind = kint), intent(in) :: i_dest, i_origin
      type(node_data), intent(in) :: nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(phys_data), intent(inout) :: phys_dest
      integer(kind = kint) :: inod
!
!     initialize
!
      call verify_vector_for_solver(n_sym_tensor, node1%numnod)
      call verify_2nd_iccg_matrix(n_sym_tensor, nod_dest%numnod)
!
!
!$omp parallel do
      do inod = 1, node1%numnod
        x_vec(6*inod-5) = d_nod(inod,i_origin  )
        x_vec(6*inod-4) = d_nod(inod,i_origin+1)
        x_vec(6*inod-3) = d_nod(inod,i_origin+2)
        x_vec(6*inod-2) = d_nod(inod,i_origin+3)
        x_vec(6*inod-1) = d_nod(inod,i_origin+4)
        x_vec(6*inod  ) = d_nod(inod,i_origin+5)
      end do
!$omp end parallel do
!
!    interpolation
      call interpolate_mod_6(comm_dest, node1%numnod, nod_dest%numnod,  &
     &    x_vec(1), xvec_2nd(1))
!
!$omp parallel do
      do inod = 1, nod_dest%numnod
        phys_dest%d_fld(inod,i_dest  ) = xvec_2nd(6*inod-5)
        phys_dest%d_fld(inod,i_dest+1) = xvec_2nd(6*inod-4)
        phys_dest%d_fld(inod,i_dest+2) = xvec_2nd(6*inod-3)
        phys_dest%d_fld(inod,i_dest+3) = xvec_2nd(6*inod-2)
        phys_dest%d_fld(inod,i_dest+4) = xvec_2nd(6*inod-1)
        phys_dest%d_fld(inod,i_dest+5) = xvec_2nd(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine s_interpolate_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_interpolate_fields(numdir, i_dest, i_origin,         &
     &          comm_dest, nod_dest, phys_dest)
!
      use m_array_for_send_recv
      use m_2nd_pallalel_vector
      use interpolate_by_module
!
!
      integer(kind = kint), intent(in) :: numdir, i_dest, i_origin
      type(node_data), intent(in) :: nod_dest
      type(communication_table), intent(in) :: comm_dest
      type(phys_data), intent(inout) :: phys_dest
!
      integer(kind = kint) :: inod, nd
!
!     initialize
!
      call verify_vector_for_solver(numdir, node1%numnod)
      call verify_2nd_iccg_matrix(numdir, nod_dest%numnod)
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, node1%numnod
          x_vec(numdir*(inod-1)+nd) = d_nod(inod,i_origin+nd-1)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
!    interpolation
      call interpolate_mod_N(comm_dest, numdir, nod_dest%numnod,        &
     &                       numdir, x_vec(1), xvec_2nd(1))
!
!$omp parallel private(inod)
      do nd = 1, numdir
!$omp do
        do inod = 1, nod_dest%numnod
          phys_dest%d_fld(inod,i_dest+nd-1)                             &
     &         = xvec_2nd(numdir*(inod-1)+nd)
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
