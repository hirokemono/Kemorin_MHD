!
!      module m_matrix_work
!
!       subroutine  allocate_IVECT_rcm(numnod)
!       subroutine  allocate_iW_ordering(numnod)
!       subroutine  allocate_work_4_rcm(numnod, NHYP)
!       subroutine  allocate_work_num_4_djds(numnod)
!       subroutine  allocate_work_4_djds(numnod, NHYP, npLX1, npUX1)
!       subroutine  allocate_conn_work_4_djds(internal_node,            &
!     &            NLmax, NUmax)
!
!       subroutine reset_4_new_rcm
!
!       subroutine deallocate_IVECT_rcm
!       subroutine deallocate_iW_ordering
!       subroutine deallocate_work_4_RCM
!       subroutine deallocate_work_4_djds
!       subroutine deallocate_4_IVECmc
!
!       subroutine check_istack_and_items_mc(my_rank, internal_node)
!
      module m_matrix_work
!
      use m_precision
!
      implicit none
!C
!C-- GENERAL SCALARs
      integer(kind=kint) :: NCOLORtot

!C
!C-- MATRIX arrays
!
      integer(kind=kint), allocatable :: INLmc(:),   INUmc(:)
      integer(kind=kint), allocatable :: IALmc(:,:), IAUmc(:,:)

      integer(kind=kint), allocatable :: IWKX(:,:)

      integer(kind=kint), allocatable :: IW(:)

      integer(kind=kint), allocatable :: OLDtoNEWmc(:)
      integer(kind=kint), allocatable :: NEWtoOLDmc(:)

      integer(kind=kint), allocatable :: ICHK(:)
      integer(kind=kint), allocatable :: IVECmc(:), IVnew(:)
!
      integer(kind=kint), allocatable :: inumDJDS_L(:)
      integer(kind=kint), allocatable :: inumDJDS_U(:)
!
      integer(kind=kint), allocatable :: IVECT_rcm(:)
!
!  --   arrays for special preconditioning
!
      real(kind=kreal), allocatable :: aiccg_l_r(:,:)
      real(kind=kreal), allocatable :: aiccg_u_r(:,:)
      real(kind=kreal), allocatable :: diag_r(:,:)
      real(kind=kreal), allocatable :: index_l_r(:,:)
      real(kind=kreal), allocatable :: index_u_r(:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine  allocate_IVECT_rcm(numnod)
!
      integer (kind = kint), intent(in) :: numnod
!
      allocate ( IVECT_rcm(0:numnod) )
      IVECT_rcm(0:numnod) = 0
!
      end subroutine  allocate_IVECT_rcm
!
! ------------------------------------------
!
      subroutine  allocate_iW_ordering(numnod)
!
      integer (kind = kint), intent(in) :: numnod
!
      allocate ( IW(numnod) )
      if(numnod .gt. 0) IW(1:numnod) = 0
!
      end subroutine  allocate_iW_ordering
!
! ------------------------------------------
!
       subroutine  allocate_work_4_rcm(numnod, NHYP)
!
       integer (kind = kint), intent(in)  :: numnod, NHYP
!
      allocate (ICHK(NHYP))
      allocate (OLDtoNEWmc(numnod), NEWtoOLDmc(numnod))
      allocate (IVECmc(0:NCOLORtot))
      allocate (IVnew(numnod))

      if(NHYP .gt. 0) ICHK(1:NHYP) = 0
      if(numnod .gt. 0) then
        IVnew(1:numnod) = 0
        NEWtoOLDmc(1:numnod) = 0
        OLDtoNEWmc(1:numnod) = 0
      end if
      IVECmc(0:NCOLORtot) = 0

       end subroutine  allocate_work_4_rcm
!
! ------------------------------------------
!
       subroutine  allocate_work_num_4_djds(numnod)
!
       integer (kind = kint), intent(in)  :: numnod
!
       allocate (INLmc(numnod))
       allocate (INUmc(numnod))
!
       if(numnod .gt. 0) then
         INLmc(1:numnod) = 0
         INUmc(1:numnod) = 0
       end if

       end subroutine  allocate_work_num_4_djds
!
!
! ------------------------------------------
!
       subroutine  allocate_work_4_djds(numnod, NHYP, npLX1, npUX1)
!
       integer (kind = kint), intent(in)  :: numnod
       integer (kind=kint), intent(in) :: NHYP, npLX1, npUX1
!
!
       allocate (IWKX(0:numnod,3))
       allocate (IW(0:numnod))
!
       allocate (inumDJDS_L(0:npLX1*NHYP))
       allocate (inumDJDS_U(0:npUX1*NHYP))
!
       IWKX(0:numnod,1:3) = 0
       IW(0:numnod) =   0
!
       inumDJDS_L(0:npLX1*NHYP) = 0
       inumDJDS_U(0:npUX1*NHYP) = 0
!

       end subroutine  allocate_work_4_djds
!
! ------------------------------------------
!
       subroutine allocate_conn_work_4_djds(internal_node,              &
      &           NLmax, NUmax)
!
       integer (kind = kint), intent(in) :: internal_node
       integer (kind = kint), intent(in) :: NLmax, NUmax
!
       allocate ( IALmc(internal_node,NLmax) )
       allocate ( IAUmc(internal_node,NUmax) )
!
       if(internal_node .gt. 0) then
         if(NLmax .gt. 0) IALmc = 0
         if(NUmax .gt. 0) IAUmc = 0
       end if
!
       end subroutine  allocate_conn_work_4_djds
!
! ------------------------------------------
!
       subroutine reset_4_new_rcm
!
       deallocate (OLDtoNEWmc, NEWtoOLDmc)
       deallocate (ICHK)
       deallocate (IVECmc, IVnew)
!
       end subroutine reset_4_new_rcm
!
! ------------------------------------------
!
      subroutine  deallocate_IVECT_rcm
!
      deallocate ( IVECT_rcm )
!
      end subroutine  deallocate_IVECT_rcm
!
! ------------------------------------------
!
       subroutine deallocate_iW_ordering
!
       deallocate (IW)
!
       end subroutine deallocate_iW_ordering
!
! ------------------------------------------
!
       subroutine deallocate_work_4_RCM
!
       deallocate (ICHK, IVnew)
!
       end subroutine deallocate_work_4_RCM
!
! ------------------------------------------
!
       subroutine deallocate_work_4_djds
!
       deallocate (OLDtoNEWmc, NEWtoOLDmc)
       deallocate (INLmc, INUmc)
       deallocate (IALmc, IAUmc)
       deallocate (IWKX)
       deallocate (inumDJDS_L, inumDJDS_U)
!
       end subroutine deallocate_work_4_djds
!
! ------------------------------------------
!
       subroutine deallocate_4_IVECmc
!
       deallocate (IVECmc)
!
       end subroutine deallocate_4_IVECmc
!
! ------------------------------------------
!
       subroutine check_istack_and_items_mc(my_rank, internal_node)
!
       integer (kind = kint), intent(in) :: my_rank
       integer (kind = kint), intent(in) :: internal_node
!
       integer (kind = kint) :: i, j
!
        do i = 1, internal_node
          write(50+my_rank,*) 'IALmc', i, INLmc(i)
          write(50+my_rank,'(10i8)') (IALmc(i,j),j=1,INLmc(i))
        end do
        do i = 1, internal_node
          write(50+my_rank,*) 'IAUmc', i, INUmc(i)
          write(50+my_rank,'(10i8)') (IAUmc(i,j),j=1,INUmc(i))
        end do
!
       end subroutine check_istack_and_items_mc
!
! ------------------------------------------
!
      end module m_matrix_work
