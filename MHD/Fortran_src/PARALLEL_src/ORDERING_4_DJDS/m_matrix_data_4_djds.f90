!>@file   m_matrix_data_4_djds.f90
!!@brief  module m_matrix_data_4_djds
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2002
!
!>      DJDS matrix data
!!
!!@verbatim
!!      subroutine allocate_matrix_data_4_djds
!!      subroutine allocate_vector_data_4_djds
!!      subroutine deallocate_matrix_data_4_djds
!!      subroutine deallocate_vector_data_4_djds
!!
!!      subroutine copy_paramters_4_djds
!!
!!      subroutine check_djds_matrix_components(my_rank)
!!@endverbatim
!
      module m_matrix_data_4_djds
!
      use m_precision
!
      implicit none
!
      integer(kind=kint ) ::  NB_djds

      real(kind=kreal), allocatable :: aiccg(:)
!   coefficients of matrix
      integer (kind = kint) :: im_d, im_l, im_u
!   pointer for diagonal component, lower and upper part of matrix
      integer (kind = kint) :: num_mat_comp
!   total number of component
      real(kind=kreal), allocatable :: ALUG_L(:), ALUG_U(:)
!    coefs for preconditioning
!
      real(kind=kreal), allocatable :: b_djds(:), x_djds(:)
!    RHS and solution vector
!
      character(len=kchara) :: SOLVER_TYPE_djds
!
      real(kind = kreal) :: rtime, starttime, endtime
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_matrix_data_4_djds
!
       use m_geometry_parameter
       use m_solver_djds
!
!
        im_d = 1
        im_l = NB_djds*NB_djds*numnod + 1
        im_u = NB_djds*NB_djds*(numnod+itotal_l) + 1
        num_mat_comp = NB_djds*NB_djds * (numnod+itotal_u+itotal_l)
!
        allocate (aiccg(-NB_djds*NB_djds+1:num_mat_comp) )
        allocate (ALUG_U(NB_djds*NB_djds*internal_node) )
        allocate (ALUG_L(NB_djds*NB_djds*internal_node) )
!
        aiccg = 0.0d0
        ALUG_U= 1.d0
        ALUG_L= 1.d0
!
      end subroutine allocate_matrix_data_4_djds
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_vector_data_4_djds
!
       use m_geometry_parameter
!
       allocate (b_djds(NB_djds*numnod))
       allocate (x_djds(NB_djds*numnod))
!
       b_djds = 0.0d0
       x_djds = 0.0d0
!
      end subroutine allocate_vector_data_4_djds
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine deallocate_matrix_data_4_djds
!
!
        deallocate (aiccg)
        deallocate (ALUG_U)
        deallocate (ALUG_L)
!
      end subroutine deallocate_matrix_data_4_djds
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_vector_data_4_djds
!
       deallocate (b_djds)
       deallocate (x_djds)
!
      end subroutine deallocate_vector_data_4_djds
!
!  ---------------------------------------------------------------------
!
      subroutine copy_paramters_4_djds
!
       use m_iccg_parameter
       use m_geometry_parameter
       use m_solver_djds
       use m_crs_connect
       use m_crs_matrix
!
!
          eps = REALARRAY_crs(1)
          itr = INTARRAY_crs (1)
          sigma_diag = REALARRAY_crs(2)
!          sigma = REALARRAY_crs(3)
          method_4_solver =  METHOD_crs 
          precond_4_solver = PRECOND_crs
          precond_4_crank =  PRECOND_crs
          SOLVER_TYPE_djds = SOLVER_crs
!
      itotal_l =      ntot_crs_l
      itotal_u =      ntot_crs_u
      NB_djds =       NB_crs
!
       end subroutine copy_paramters_4_djds
!
!  ---------------------------------------------------------------------
!
       subroutine check_djds_matrix_components(my_rank)
!
       use m_geometry_parameter
       use m_solver_djds
!
       integer (kind = kint) :: my_rank, i, k1, k2, ist, ied
!
       do i = 1, numnod
           write(my_rank+50,*) "vector (inod) = ", i
           write(my_rank+50,'(1p5e16.8)')                               &
     &           (b_djds(NB_djds*(i-1)+k2),k2=1,NB_djds)
       end do
!
       do i = 1, numnod
         do k1 = 1, NB_djds
           ist = NB_djds*NB_djds*(i-1) + NB_djds*(k1-1) + 1
           ied = NB_djds*NB_djds*(i-1) + NB_djds*k1
           write(my_rank+50,*) "diagonal (inod,k1) = ", i, k1
           write(my_rank+50,'(1p5e16.8)') aiccg(ist:ied)
         end do
       end do
!
       do i = numnod+1, numnod+itotal_l
           do k1 = 1, NB_djds
             ist = NB_djds*NB_djds*(i-1) + NB_djds*(k1-1) + 1
             ied = NB_djds*NB_djds*(i-1) + NB_djds*k1
             write(my_rank+50,*) "Lower component (i,k1) = ",          &
     &             (i-numnod), k1
             write(my_rank+50,'(1p5e16.8)') aiccg(ist:ied)
           end do
       end do
!
       do i = numnod+itotal_l+1, numnod+itotal_l+itotal_u
           do k1 = 1, NB_djds
             ist = NB_djds*NB_djds*(i-1) + NB_djds*(k1-1) + 1
             ied = NB_djds*NB_djds*(i-1) + NB_djds*k1
             write(my_rank+50,*) "Upper component (i,k1) = ",          &
     &             (i-numnod-itotal_l), k1
             write(my_rank+50,'(1p5e16.8)') aiccg(ist:ied)
           end do
       end do
!
       end subroutine check_djds_matrix_components
!
!  ---------------------------------------------------------------------
!
      end module m_matrix_data_4_djds
