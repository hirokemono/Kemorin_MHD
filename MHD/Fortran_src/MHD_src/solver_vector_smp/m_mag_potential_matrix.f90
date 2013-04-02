!
!     module   m_mag_potential_matrix
!.......................................................................
!
!      Written by H. Matsui
!
!       subroutine allocate_aiccg_mag_p
!       subroutine reset_aiccg_mag_p
!       subroutine deallocate_aiccg_mag_p
!
!       subroutine allocate_aiccg_mag_p_ins
!       subroutine reset_aiccg_mag_p_ins
!       subroutine deallocate_aiccg_mag_p_ins
!
!!       subroutine allocate_aiccg_mag_p_cd
!!       subroutine reset_aiccg_mag_p_cd
!!       subroutine deallocate_aiccg_mag_p_cd
!
      module   m_mag_potential_matrix
!
      use m_precision
!
      implicit  none
!
      real(kind=kreal), allocatable, target :: aiccg_mag_p(:)
!   coefficients of matrix
!
!      real(kind=kreal), allocatable, target :: aiccg_mag_pi(:)
!   coefficients of matrix
!
!      real(kind=kreal), allocatable, target :: aiccg_mag_pc(:)
!   coefficients of matrix
!
      integer (kind = kint) :: im_mp_d
!   pointer for diagonal component
      integer (kind = kint) :: im_mp_u
!   pointer for upper part of matrix
      integer (kind = kint) :: im_mp_l
!   pointer for lower part of matrix
       integer (kind = kint) :: num_mp_comp
!   total number of component
!
!      integer (kind = kint) :: im_mpi_d
!   pointer for diagonal component
!      integer (kind = kint) :: im_mpi_u
!   pointer for upper part of matrix
!      integer (kind = kint) :: im_mpi_l
!   pointer for lower part of matrix
!       integer (kind = kint) :: num_mpi_comp
!   total number of component
!
!      integer (kind = kint) :: im_mpc_d
!   pointer for diagonal component
!      integer (kind = kint) :: im_mpc_u
!   pointer for upper part of matrix
!      integer (kind = kint) :: im_mpc_l
!   pointer for lower part of matrix
!       integer (kind = kint) :: num_mpc_comp
!   total number of component
!
!
      real(kind=kreal), allocatable, target :: ALUG_mag_p_u(:)
      real(kind=kreal), allocatable, target :: ALUG_mag_p_l(:)
! 
!
!      real(kind=kreal), allocatable, target :: ALUG_mag_pi_u(:)
!      real(kind=kreal), allocatable, target :: ALUG_mag_pi_l(:)
!
!
!      real(kind=kreal), allocatable, target :: ALUG_mag_pc_u(:)
!      real(kind=kreal), allocatable, target :: ALUG_mag_pc_l(:)
! 
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
       subroutine allocate_aiccg_mag_p
!
       use m_geometry_parameter
       use m_geometry_data
       use m_geometry_data_MHD
       use m_solver_djds_linear
!
       im_mp_d = 1
       im_mp_l = numnod + 1
       im_mp_u = numnod + itotal1_l + 1
!
       num_mp_comp = numnod + itotal1_u + itotal1_l
!
       allocate(aiccg_mag_p(0:num_mp_comp) )
!
       allocate (ALUG_mag_p_U(internal_node) )
       allocate (ALUG_mag_p_L(internal_node) )
!
       call reset_aiccg_mag_p
!
       end subroutine allocate_aiccg_mag_p
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_mag_p
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_linear
!
      integer (kind = kint) :: in, inod, iele, k1
!
      aiccg_mag_p = 0.0d0
!
      do inod = 1, numnod
        aiccg_mag_p(inod) = 1.0d0
      end do
!
      do k1 = 1, num_t_linear
        do iele = 1, numele
          inod = ie(iele,k1)
          in = OLDtoNEW1(inod)
          aiccg_mag_p(in) = 0.0d0
        end do
      end do
!
      ALUG_mag_p_U= 1.d0
      ALUG_mag_p_L= 1.d0
!
      end subroutine reset_aiccg_mag_p
!
! ----------------------------------------------------------------------
!
       subroutine deallocate_aiccg_mag_p
!
!
       deallocate(aiccg_mag_p )
!
       deallocate (ALUG_mag_p_U )
       deallocate (ALUG_mag_p_L )
!
       end subroutine deallocate_aiccg_mag_p
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!       subroutine allocate_aiccg_mag_p_ins
!
!       use m_geometry_parameter
!       use m_geometry_data
!       use m_geometry_data_MHD
!       use m_solver_djds_linear_ins
!
!
!       im_mpi_d = 1
!       im_mpi_l = numnod + 1
!       im_mpi_u = numnod + itotal1_ins_l + 1
!
!       num_mpi_comp = numnod + itotal1_ins_u + itotal1_ins_l
!
!       allocate(aiccg_mag_pi(num_mpi_comp) )
!
!       allocate (ALUG_mag_pi_U(internal_node) )
!       allocate (ALUG_mag_pi_L(internal_node) )
!
!       call reset_aiccg_mag_p_ins
!
!
!       end subroutine allocate_aiccg_mag_p_ins
!
! ----------------------------------------------------------------------
!
!      subroutine reset_aiccg_mag_p_ins
!
!      use m_geometry_constants
!      use m_geometry_parameter
!      use m_geometry_data
!      use m_geometry_data_MHD
!      use m_solver_djds_linear_ins
!
!      integer(kind = kint) :: in, inod, iele, k1
!
!
!      aiccg_mag_pi = 0.0d0
!
!      do inod = 1, numnod
!        aiccg_mag_pi(inod) = 1.0d0
!      end do
!
!      do k1 = 1, num_t_linear
!        do iele = iele_ins_start, iele_ins_end
!          inod = ie(iele,k1)
!          in = OLDtoNEW1(inod)
!          aiccg_mag_pi(in) = 0.0d0
!        end do
!      end do
!
!      ALUG_mag_pi_U= 0.d0
!      ALUG_mag_pi_L= 0.d0
!
!
!      end subroutine reset_aiccg_mag_p_ins
!
! ----------------------------------------------------------------------
!
!       subroutine deallocate_aiccg_mag_p_ins
!
!
!       deallocate(aiccg_mag_pi )
!
!       deallocate (ALUG_mag_pi_U )
!       deallocate (ALUG_mag_pi_L )
!
!       end subroutine deallocate_aiccg_mag_p_ins
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!      subroutine allocate_aiccg_mag_p_cd
!
!      use m_geometry_parameter
!      use m_geometry_data
!      use m_geometry_data_MHD
!      use m_solver_djds_linear_cd
!
!      im_mpc_d = 1
!      im_mpc_l = numnod + 1
!      im_mpc_u = numnod + itotal1_cd_l + 1
!
!      num_mpc_comp = numnod + itotal1_cd_u + itotal1_cd_l
!
!      allocate(aiccg_mag_pc(num_mpc_comp) )
!
!      allocate (ALUG_mag_pc_U(internal_node) )
!      allocate (ALUG_mag_pc_L(internal_node) )
!
!      call reset_aiccg_mag_p_cd
!
!      end subroutine allocate_aiccg_mag_p_cd
!
! ----------------------------------------------------------------------
!
!      subroutine reset_aiccg_mag_p_cd
!
!      use m_geometry_constants
!      use m_geometry_parameter
!      use m_geometry_data
!      use m_geometry_data_MHD
!      use m_solver_djds_linear_cd
!
!      integer(kind = kint) :: inod, iele, in, k1
!
!      aiccg_mag_pc = 0.0d0
!
!      do inod = 1, numnod
!        aiccg_mag_pc(inod) = 1.0d0
!      end do
!
!      do k1 = 1, num_t_linear
!        do iele = iele_cd_start, iele_cd_end
!          inod = ie(iele,k1)
!          in = OLDtoNEW1(inod)
!          aiccg_mag_pc(in) = 0.0d0
!        end do
!      end do
!
!      ALUG_mag_pc_U= 0.d0
!      ALUG_mag_pc_L= 0.d0
!
!      end subroutine reset_aiccg_mag_p_cd
!
! ----------------------------------------------------------------------
!
!       subroutine deallocate_aiccg_mag_p_cd
!
!
!       deallocate(aiccg_mag_pc )
!
!       deallocate (ALUG_mag_pc_U )
!       deallocate (ALUG_mag_pc_L )
!
!       end subroutine deallocate_aiccg_mag_p_cd
!
! ----------------------------------------------------------------------
!
      end module m_mag_potential_matrix
