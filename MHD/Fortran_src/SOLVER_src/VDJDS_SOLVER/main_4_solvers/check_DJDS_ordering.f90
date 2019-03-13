!
!      module check_DJDS_ordering
!
!     Written by Kemorin
!
!      subroutine deallocate_check_djds_array
!       subroutine s_check_DJDS_ordering                                &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, NtoO,     &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            id_rank)
!
!       subroutine reverse_DJDS_matrix                                  &
!     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,        &
!     &             PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, NtoO,    &
!     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,    &
!     &            d, al, au, id_rank)
!
      module check_DJDS_ordering
!
      use m_precision
!
      implicit none
!
       integer(kind = kint), allocatable :: iX(:), iW(:,:)
       real(kind = kreal), allocatable :: a_check(:,:)
!
       integer(kind = kint) :: ntot_l, ntot_u
       integer(kind = kint), allocatable :: istack_l_crs(:)
       integer(kind = kint), allocatable :: istack_u_crs(:)
       integer(kind = kint), allocatable :: item_l_crs(:)
       integer(kind = kint), allocatable :: item_u_crs(:)
       real(kind = kreal), allocatable :: d_crs(:)
       real(kind = kreal), allocatable :: al_crs(:)
       real(kind = kreal), allocatable :: au_crs(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_check_djds_array
!
      deallocate(iX, iW)
      deallocate(a_check)
!
      deallocate(istack_l_crs, istack_u_crs, item_l_crs, item_u_crs)
      deallocate(d_crs, al_crs, au_crs)
!
      end subroutine deallocate_check_djds_array
!
!  ---------------------------------------------------------------------
!
       subroutine s_check_DJDS_ordering                                 &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &            PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, NtoO,      &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            id_rank)
!
      use order_vect_4_solver_11
      use ordering_by_l2u_11
      use ordering_by_o2nl_11
      use ordering_by_new2old_U_11
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
       integer, intent(in) :: id_rank
!
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
!
!
      allocate(iX(NP), iW(NP,2))
      iX = 0
      iW = 0
!
       do i = 1, NP
         iX(i) = i
       end do
!
       call change_order_2_solve_int(NP, PEsmpTOT, STACKmcG,            &
     &     NtoO, iX, iW(1,1))
!
      write(id_rank+50,'(a)') 'ordering for solver(Multicolor)'
      write(id_rank+50,'(a)') 'ip, i, iX(i)'
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do i= iS, iE
          write(id_rank+50,*) ip, i, iX(i)
        end do
      end do
!
      call ordering_int_by_old2new_L(NP, PEsmpTOT, STACKmcG,            &
     &    OtoN_L, iW(1,1), iX)
       if(NP.gt.N) iW(N+1:NP,1) = iX(N+1:NP)

      write(id_rank+50,'(a)') 'ordering for lower triangle'
      do iv= 1, NVECT
        write(id_rank+50,'(a)') 'iv, ip, i, iW(i,1), IAL(k)'
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
            do i= iv0+1, iv0+iE-iS
              k  = i+iS - iv0
              write(id_rank+50,*) iv, ip, i, iW(i,1), IAL(k)
            end do
          end do
        end do
      end do

      call ordering_int_by_l2u(NP, LtoU, iW(1,2), iW(1,1))

      write(id_rank+50,'(a)') 'ordering for upper triangle'
      do iv= 1, NVECT
        write(id_rank+50,'(a)') 'iv, ip, i, iW(i,2), IAU(k)'
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              write(id_rank+50,*) iv, ip, i, iW(i,2), IAU(k)
            end do
          end do
        end do
      end do

      call ordering_int_by_new2old_U(NP, PEsmpTOT, STACKmcG,            &
     &    NtoO_U, iX, iW(1,2) )
       if(NP.gt.N) iX(N+1:NP) = iW(N+1:NP,1)
!
      write(id_rank+50,'(a)') 'finish non-digonal part'
      write(id_rank+50,'(a)') 'ip, i, iX(i)'
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do i= iS, iE
          write(id_rank+50,*) ip, i, iX(i)
        end do
      end do
!
       call back_2_original_order_int(NP, NtoO, iX, iW(1,1) )
!
      write(id_rank+50,'(a)') 'Returned index'
      write(id_rank+50,'(a)') 'ip, i, iX(i)'
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do i= iS, iE
          write(id_rank+50,*) ip, i, iX(i)
        end do
      end do
!
      deallocate(iX, iW)
!
      end subroutine s_check_DJDS_ordering
!
!  ---------------------------------------------------------------------
!
       subroutine reverse_DJDS_matrix                                   &
     &           (N, NP, NL, NU, NPL, NPU, npLX1, npUX1, NVECT,         &
     &             PEsmpTOT, STACKmcG, STACKmc, NLhyp, NUhyp, NtoO,     &
     &            OtoN_L, OtoN_U, NtoO_U, LtoU, INL, INU, IAL, IAU,     &
     &            d, al, au, id_rank)
!
      use order_vect_4_solver_11
      use ordering_by_l2u_11
      use ordering_by_o2nl_11
      use ordering_by_new2old_U_11
!
       integer(kind = kint), intent(in) :: N, NP, PEsmpTOT
       integer(kind = kint), intent(in) :: NL, NU, NPL, NPU
       integer(kind = kint), intent(in) :: NVECT, npLX1, npUX1
       integer(kind = kint), intent(in) :: NLhyp(NVECT)
       integer(kind = kint), intent(in) :: NUhyp(NVECT)
       integer(kind = kint), intent(in) :: STACKmcG(0:PEsmpTOT)
       integer(kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
       integer(kind = kint), intent(in) :: NtoO(NP)
       integer(kind = kint), intent(in) :: OtoN_L(NP), OtoN_U(NP)
       integer(kind = kint), intent(in) :: NtoO_U(NP), LtoU(NP)
       integer(kind = kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: INU(0:NU*NVECT*PEsmpTOT)
       integer(kind = kint), intent(in) :: IAL(NPL)
       integer(kind = kint), intent(in) :: IAU(NPU)
       integer, intent(in) :: id_rank
       real(kind = kreal), intent(in) :: d(NP), al(NPL), au(NPU)
!
       integer (kind = kint) :: ip, iS, iE, iv0, iv, i, j, k, kk
       integer(kind = kint) :: ii, jj
!
!
      allocate(iX(NP), iW(NP,2), a_check(NP,NP))
      iX = 0
      iW = 0
      a_check = 0.0d0
!
       do i = 1, NP
         iX(i) = i
       end do
!
       call change_order_2_solve_int(NP, PEsmpTOT, STACKmcG,            &
     &     NtoO, iX, iW(1,1))
!
      do ip= 1, PEsmpTOT
        iS= STACKmcG(ip-1) + 1
        iE= STACKmcG(ip  )
        do i= iS, iE
          ii = iX(i)
          a_check(ii,ii) = d(i)
        end do
      end do
!
      call ordering_int_by_old2new_L(NP, PEsmpTOT, STACKmcG,            &
     &    OtoN_L, iW(1,1), iX)
       if(NP.gt.N) iW(N+1:NP,1) = iX(N+1:NP)
!
      do iv= 1, NVECT
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NLhyp(iv)
            iS= INL(npLX1*(iv-1)+NL*(ip-1)+j-1)
            iE= INL(npLX1*(iv-1)+NL*(ip-1)+j )
            do i= iv0+1, iv0+iE-iS
              k  = i+iS - iv0
              ii =  iW(i,1)
              jj =  iW(IAL(k),1)
              a_check(ii,jj) = al(k)
            end do
          end do
        end do
      end do

      call ordering_int_by_l2u(NP, LtoU, iW(1,2), iW(1,1))

      do iv= 1, NVECT
        do ip= 1, PEsmpTOT
          iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
          do  j= 1, NUhyp(iv)
            iS= INU(npUX1*(iv-1)+NU*(ip-1)+j-1)
            iE= INU(npUX1*(iv-1)+NU*(ip-1)+j  )
            do i= iv0+1, iv0+iE-iS
              k= i+iS - iv0
              ii =  iW(i,2)
              jj =  iW(IAU(k),2)
              a_check(ii,jj) = au(k)
            end do
          end do
        end do
      end do

!
      allocate( d_crs(NP) )
      allocate( istack_l_crs(0:NP) )
      allocate( istack_u_crs(0:NP) )
!
      do i = 1, NP
        d_crs(i) = a_check(i,i)
      end do
!
      istack_l_crs(0) = 0
      istack_u_crs(0) = 0
      do i = 1, NP
        istack_l_crs(i) = istack_l_crs(i-1)
        istack_u_crs(i) = istack_u_crs(i-1)
        do j = 1, i-1
          if(a_check(i,j) .ne. 0.0d0)                                   &
     &          istack_l_crs(i) = istack_l_crs(i) + 1
        end do
        do j = i+1, NP
          if(a_check(i,j) .ne. 0.0d0)                                   &
     &          istack_u_crs(i) = istack_u_crs(i) + 1
        end do
      end do
      ntot_l = istack_l_crs(NP)
      ntot_u = istack_u_crs(NP)
!
      allocate( item_l_crs(ntot_l) )
      allocate( item_u_crs(ntot_u) )
      allocate( al_crs(ntot_l) )
      allocate( au_crs(ntot_u) )
!
      do i = 1, NP
        k = istack_l_crs(i-1)
        do j = 1, i-1
          if(a_check(i,j) .ne. 0.0d0) then
            k = k + 1
            item_l_crs(k) = j
            al_crs(k) = a_check(i,j)
          end if
        end do
        k = istack_u_crs(i-1)
        do j = i+1, NP
          if(a_check(i,j) .ne. 0.0d0) then
            k = k + 1
            item_u_crs(k) = j
            au_crs(k) = a_check(i,j)
          end if
        end do
      end do
!
      write(50+id_rank,*) 'i, d_crs(i)'
      do i = 1, NP
        write(50+id_rank,*) i, d_crs(i)
      end do
!
      write(50+id_rank,*) 'i, j, item_l_crs(k), al_crs(k)'
      do i = 1, NP
        do j = istack_l_crs(i-1)+1, istack_l_crs(i)
          write(50+id_rank,*) i, j, item_l_crs(j), al_crs(j)
        end do
      end do
!
          write(50+id_rank,*) 'i, j, item_u_crs(j), au_crs(j)'
      do i = 1, NP
        do j = istack_u_crs(i-1)+1, istack_u_crs(i)
          write(50+id_rank,*) i, j, item_u_crs(j), au_crs(j)
        end do
      end do
!
      write(id_rank+50,*) 'ii, a_check(ii,1:NP)'
      do ii = 1, NP
        write(id_rank+50,*) ii, a_check(ii,1:NP)
      end do
!
      end subroutine reverse_DJDS_matrix
!
!  ---------------------------------------------------------------------
!
      end module check_DJDS_ordering
