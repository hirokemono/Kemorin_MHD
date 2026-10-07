! Synthetic physical-grid tests of the production kernel and MPI output.
program test_signed_axial_monitor
  use m_precision
  use calypso_mpi
  use calypso_mpi_real
  use transfer_to_long_integers
  use t_spheric_parameter
  use t_schmidt_poly_on_rtm
  use t_sph_trans_arrays_MHD
  use t_time_data
  use t_signed_axial_field_monitor
  implicit none
  type(sph_grids) :: sph
  type(legendre_4_sph_trans) :: leg
  type(signed_axial_field_monitor) :: mon
  type(address_4_sph_trans) :: trns
  type(time_data) :: td
  integer :: mode, field_case, k, l, p, i, n(3), counts(3), d, g
  integer :: u, ios, step, records
  real(kind=kreal) :: acc(3), total(3), expected(3), volume, bz, t, values(3)
  real(kind=kreal) :: radial(4), pi
  character(len=256) :: line
  call calypso_MPI_init
  pi = 4.0_kreal*atan(1.0_kreal)
  radial = [0.5_kreal,0.9_kreal,1.4_kreal,2.0_kreal]
  volume = 4*pi*sum(0.5_kreal*(radial(3:4)-radial(2:3)) &
                    *(radial(2:3)**2+radial(3:4)**2))
  sph%sph_rj%nidx_rj = [4,1]
  allocate(sph%sph_rj%radius_1d_rj_r(4))
  sph%sph_rj%radius_1d_rj_r = radial
  sph%sph_params%nlayer_ICB = 2
  sph%sph_params%nlayer_CMB = 4
  sph%sph_rtp%nidx_global_rtp = [4,2,4]
  allocate(leg%weight_rtm(2),leg%g_colat_rtm(2))
  leg%weight_rtm = 1.0_kreal
  leg%g_colat_rtm = acos([1.0_kreal,-1.0_kreal]/sqrt(3.0_kreal))
  trns%b_trns%base%i_magne = 1
  mon%file_prefix = 'signed_axial_test'
! Disabled monitor must require neither initialized weights nor fields.
  call output_signed_axial_monitor(td,sph%sph_rtp,trns,mon)
  mon%enabled = .true.
  do mode = 1,3
! Decompose radius, latitude, or longitude; include empty partitions.
    n = [4,2,4]
    counts = n
    counts(mode) = count([(mod(g-1,nprocs)==my_rank,g=1,n(mode))])
    sph%sph_rtp%nidx_rtp = counts
! Exercise longitude-fast storage instead of assuming radius-fast.
    sph%sph_rtp%istep_rtp = [counts(3),counts(1)*counts(3),1]
    sph%sph_rtp%nnod_rtp = product(counts)
    allocate(sph%sph_rtp%idx_gl_1d_rtp_r(counts(1)))
    allocate(sph%sph_rtp%idx_gl_1d_rtp_t(counts(2)))
    do d = 1,2
      i=0
      do g=1,n(d)
        if(d==mode .and. mod(g-1,nprocs)/=my_rank) cycle
        i=i+1
        if(d==1) sph%sph_rtp%idx_gl_1d_rtp_r(i)=g
        if(d==2) sph%sph_rtp%idx_gl_1d_rtp_t(i)=g
      end do
    end do
    allocate(trns%backward%fld_rtp(product(counts),3))
    call init_signed_axial_monitor(sph,leg,mon)
    do field_case=1,3
      do l=1,counts(2)
        do k=1,counts(1)
          bz=2.0_kreal
          if(field_case==2) bz=-2.0_kreal
          if(field_case==3) bz=mon%ct(l)
          do p=1,counts(3)
            i=1+(k-1)*sph%sph_rtp%istep_rtp(1) &
               +(l-1)*sph%sph_rtp%istep_rtp(2)+(p-1)
            trns%backward%fld_rtp(i,:)=[bz*mon%ct(l),-bz*mon%st(l),99.0_kreal]
          end do
        end do
      end do
      call cal_signed_axial_integrals(sph%sph_rtp,1_kint,trns%backward%fld_rtp,mon,acc)
      call calypso_mpi_allreduce_real(acc,total,cast_long(3_kint),MPI_SUM)
      if(field_case==1) expected=[2*volume,2*volume,0.0_kreal]
      if(field_case==2) expected=[-2*volume,0.0_kreal,2*volume]
      if(field_case==3) expected=[0.0_kreal,volume/(2*sqrt(3.0_kreal)), &
                                               volume/(2*sqrt(3.0_kreal))]
      if(maxval(abs(total-expected))>1.e-12_kreal*volume) error stop 'integral'
      if(abs(total(1)-total(2)+total(3))>1.e-12_kreal*volume) error stop 'closure'
      td%i_time_step=(mode-1)*3+field_case
      td%time=0.1_kreal*td%i_time_step
      call output_signed_axial_monitor(td,sph%sph_rtp,trns,mon)
    end do
    deallocate(sph%sph_rtp%idx_gl_1d_rtp_r,sph%sph_rtp%idx_gl_1d_rtp_t)
    deallocate(trns%backward%fld_rtp)
  end do
  if(my_rank==0) then
    records=0
    open(newunit=u,file='signed_axial_test.dat',status='old')
    do
      read(u,'(a)',iostat=ios) line
      if(ios/=0) exit
      if(line(1:1)=='#') cycle
      read(line,*) step,t,values
      records=records+1
      if(step/=records .or. abs(t-0.1_kreal*step)>1.e-14_kreal) error stop 'time'
      if(abs(values(1)-values(2)+values(3))>1.e-12_kreal*volume) error stop 'file'
    end do
    close(u)
    if(records/=9) error stop 'record count'
    print *, 'PASS: signed axial integrals, MPI partitions, timestamps and output'
  end if
  call calypso_MPI_finalize
end program
