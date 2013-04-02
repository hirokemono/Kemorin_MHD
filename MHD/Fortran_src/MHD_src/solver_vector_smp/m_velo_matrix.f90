!
!     module   m_velo_matrix
!.......................................................................
!
!      Written by H. Matsui
!
      module   m_velo_matrix
!
      use m_precision
!
      implicit  none
!
!
      real(kind=kreal), allocatable, target :: aiccg_velo(:)
!   coefficients of matrix
!
      integer (kind = kint) :: im_velo_d
!   pointer for diagonal component
      integer (kind = kint) :: im_velo_u
!   pointer for upper part of matrix
      integer (kind = kint) :: im_velo_l
!   pointer for lower part of matrix
      integer (kind = kint) :: num_velo_comp
!   total number of component
!
      real(kind=kreal), allocatable, target :: ALUG_velo_L(:)
      real(kind=kreal), allocatable, target :: ALUG_velo_U(:)
!
!      subroutine allocate_aiccg_velo
!      subroutine deallocate_aiccg_velo
!      subroutine reset_aiccg_velo
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_aiccg_velo
!
       use m_geometry_parameter
       use m_solver_djds_fluid
!
!
       im_velo_d = 1
       im_velo_l = 9*numnod + 1
       im_velo_u = 9*(numnod+itotal_fl_l) + 1
!
       num_velo_comp = 9 * (numnod+itotal_fl_u+itotal_fl_l)
!
       allocate(aiccg_velo(-8:num_velo_comp) )
       allocate (ALUG_velo_U(9*internal_node) )
       allocate (ALUG_velo_L(9*internal_node) )
!
       call reset_aiccg_velo
!
       end subroutine allocate_aiccg_velo
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_aiccg_velo
!
!
       deallocate(aiccg_velo  )
       deallocate(ALUG_velo_U )
       deallocate(ALUG_velo_L )
!
       end subroutine deallocate_aiccg_velo
!
! ----------------------------------------------------------------------
!
      subroutine reset_aiccg_velo
!
      use m_geometry_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_solver_djds_fluid
!
      integer (kind = kint) :: inod, iele, k1
      integer (kind = kint) :: nd, in
!
!
      aiccg_velo = 0.0d00
!
      do inod = 1, numnod
        aiccg_velo(inod*9-8) = 1.0d0
        aiccg_velo(inod*9-4) = 1.0d0
        aiccg_velo(inod*9  ) = 1.0d0
      end do
!
      do k1 = 1, nnod_4_ele
        do iele = iele_fl_start, iele_fl_end
          inod = ie(iele,k1)
          in = OLDtoNEW(inod)
          aiccg_velo(in*9-8) = 0.0d0
          aiccg_velo(in*9-4) = 0.0d0
          aiccg_velo(in*9  ) = 0.0d0
        end do
      end do
!
      ALUG_velo_U= 1.d0
      ALUG_velo_L= 1.d0
!
      end subroutine reset_aiccg_velo
!
! ----------------------------------------------------------------------
!
      end module m_velo_matrix
!
