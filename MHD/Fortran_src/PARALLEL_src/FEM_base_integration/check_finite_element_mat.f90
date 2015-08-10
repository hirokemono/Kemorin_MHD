!check_finite_element_mat.f90
!     module check_finite_element_mat
!
!     Written by H. Matsui on Aug., 2005
!
!      subroutine check_mass_martix
!      subroutine check_mass_martix_fluid
!      subroutine check_mass_martix_conduct
!      subroutine check_mass_martix_insulate
!      subroutine check_ff(numdir)
!      subroutine check_ff_nl(numdir)
!      subroutine check_ff_smp(numdir)
!      subroutine check_ff_nl_smp(numdir)
!      subroutine check_ff_m_smp(numdir)
!      subroutine check_sk6
!
      module check_finite_element_mat
!
      use m_precision
!
      use calypso_mpi
      use m_geometry_data
      use m_finite_element_matrix
!
      implicit  none
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml, ml_o'
      do inod = 1, node1%numnod
        write(50+my_rank,'(i16,1p2e25.14)') inod, ml(inod), ml_o(inod)
      end do
!
      end subroutine check_mass_martix
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_fluid
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml_fl, ml_o'
      do inod = 1, node1%numnod
        write(50+my_rank,'(i16,1p2e25.14)')                             &
     &           inod, ml_fl(inod), ml_o_fl(inod)
      end do
!
      end subroutine check_mass_martix_fluid
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_conduct
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml_cd, ml_o_cd'
      do inod = 1, node1%numnod
        write(50+my_rank,'(i16,1p2e25.14)')                             &
     &       inod, ml_cd(inod), ml_o_cd(inod)
      end do
!
      end subroutine check_mass_martix_conduct
!
!   ---------------------------------------------------------------------
!
      subroutine check_mass_martix_insulate
!
      integer(kind = kint) :: inod
!
      write(50+my_rank,*) 'inod, ml_ins, ml_o_ins'
      do inod = 1, node1%numnod
        write(50+my_rank,'(i16,1p2e25.14)')                             &
     &        inod, ml_ins(inod), ml_ins(inod)
      end do
!
      end subroutine check_mass_martix_insulate
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff(numdir)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint) :: inod, nd
!
      write(50+my_rank,*) 'inod, ff', numdir
      do inod = 1, node1%numnod
        write(50+my_rank,'(i16,1p10e25.14)')                            &
     &         inod, (ff(inod,nd),nd=1, numdir)
      end do
!
      end subroutine check_ff
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff_nl(numdir)
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint) :: inod, nd
!
      write(50+my_rank,*) 'inod, ff_nl', numdir
      do inod = 1, node1%numnod
        write(50+my_rank,'(i16,1p10e25.14)')                            &
     &         inod, (ff_nl(inod,nd),nd=1, numdir)
      end do
!
      end subroutine check_ff_nl
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff_smp(numdir)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint) :: ip, inod, nd
!
      write(50+my_rank,*) 'ip, inod, ff_smp', numdir
      do ip = 1, np_smp
       do inod = 1, maxnod_4_smp
        write(50+my_rank,'(2i16,1p10e25.14)')                           &
     &         ip, inod, (ff_smp(inod,nd,ip),nd=1, numdir)
      end do
      end do
!
      end subroutine check_ff_smp
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff_nl_smp(numdir)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint) :: ip, inod, nd
!
      write(50+my_rank,*) 'ip, inod, ff_nl_smp', numdir
      do ip = 1, np_smp
       do inod = 1, maxnod_4_smp
        write(50+my_rank,'(2i16,1p10e25.14)')                           &
     &         ip, inod, (ff_nl_smp(inod,nd,ip),nd=1, numdir)
      end do
      end do
!
      end subroutine check_ff_nl_smp
!
!   ---------------------------------------------------------------------
!
      subroutine check_ff_m_smp(numdir)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint) :: ip, inod, nd
!
      write(50+my_rank,*) 'ip, inod, ff_m_smp', numdir
      do ip = 1, np_smp
       do inod = 1, maxnod_4_smp
        write(50+my_rank,'(2i16,1p10e25.14)')                           &
     &         ip, inod, (ff_m_smp(inod,nd,ip),nd=1, numdir)
      end do
      end do
!
      end subroutine check_ff_m_smp
!
!   ---------------------------------------------------------------------
!
      subroutine check_sk6
!
      integer(kind = kint) :: iele, k1
!
      write(50+my_rank,*) 'k1, iele, sk6'
      do k1 = 1, nnod_4_ele
       do iele = 1, numele
        write(50+my_rank,'(2i16,1p10e25.14)')                           &
     &         k1, iele, sk6(iele,1:6,k1)
       end do
      end do
!
      end subroutine check_sk6
!
!   ---------------------------------------------------------------------
!
      end module check_finite_element_mat
